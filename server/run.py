import re
import os
import json
from requests_oauthlib import OAuth2Session
from flask import Flask, render_template, request, redirect, url_for, session, jsonify
import interp_v2
import interp_v3
from forthic.v3.modules.ui_module import ForthicReact

# Forthic utils
from forthic.utils.creds import (
    Creds,
    MissingSecretsFile,
    MissingPasswordCreds,
    MissingAppCreds,
    MissingOAuthToken,
)
from forthic.utils.errors import UnauthorizedError

# Allow us to use http locally
os.environ['OAUTHLIB_INSECURE_TRANSPORT'] = '1'

SECRETS_DIR = '.'

creds = Creds(SECRETS_DIR)
creds.ensure_key()

app = Flask(__name__)
app.config['TEMPLATES_AUTO_RELOAD'] = True
app.secret_key = creds.get_key()


class UnknownInterpreterVersion(RuntimeError):
    def __init__(self, app_dir, forthic_version):
        super().__init__(f"Can't find Forthic version {forthic_version} for app {app_dir}")


@app.after_request
def add_header(response):
    response.cache_control.max_age = 0
    return response


# # ===== Routes =================================================================
@app.route('/')
def home():
    coding_forthic_apps = os.listdir("./apps/coding-forthic")
    test_apps = os.listdir("./apps/tests")
    apps = {
        "coding-forthic": coding_forthic_apps,
        "tests": test_apps
    }
    return render_template('home.html', apps=apps)


@app.route('/<group>/<app>/')
@app.route('/<group>/<app>/<path:rest>')
def forthic_app(group, app, rest=None):
    """
    Renders a Forthic app at the specified group/app directory
    """
    app_directory = f"./apps/{group}/{app}"
    interp = get_interp(app_directory)

    try:
        run_app_forthic(interp, app_directory)
        interp.run("MAIN-PAGE")
        main_page = interp.stack_pop()

    # The secrets file is where we store all passwords/tokens/credentials (encrypted)
    except MissingSecretsFile:
        creds.ensure_secrets_file()
        return redirect(url_for('forthic_app', group=group, app=app, rest=rest))

    # Some credentials require a username and password
    except (MissingPasswordCreds, UnauthorizedError) as e:
        return redirect(url_for('update_password_form', group=group, app=app, field=e.field, rest=rest))

    # For some services (like Google), we need to authenticate our Forthic app itself so we can access the APIs
    except MissingAppCreds as e:
        return redirect(url_for('update_app_creds_form', group=group, app=app, field=e.field, rest=rest))

    # Some services require an OAuth/access token
    except MissingOAuthToken as e:
        session['group'] = group
        session['app'] = app
        session["rest"] = rest
        if e.field == 'GOOGLE_TOKEN':
            return redirect(get_google_auth_url())
        else:
            raise RuntimeError(f'Unknown OAuth token type: {e.field}')

    # At this point, we were able to run the app Forthic and "MAIN-PAGE"
    if isinstance(main_page, str):
        result = render_template('basic.html', main_page=main_page)
    elif isinstance(main_page, ForthicReact):
        result = render_template(
            get_forthic_react_template(main_page),
            css=main_page.css,
            jsx=main_page.jsx,
            forthic=main_page.forthic,
            basename=f"/{group}/{app}")
    else:
        raise RuntimeError(f"Unable to render main_page: {main_page}")

    return result



@app.route('/<group>/<app>/forthic', methods=["POST"])
def run_forthic(group, app):
    app_directory = f"./apps/{group}/{app}"
    interp = get_interp(app_directory)

    form = request.form
    if request.is_json:
        form = request.json
    forthic = form['forthic']
    fullstack_response = form.get('fullstack_response')

    def run_forthic():
        run_app_forthic(interp, app_directory)
        interp.run(forthic)
        try:
            if fullstack_response:
                res = interp.stack
            else:
                res = interp.stack_pop()
        except RuntimeError:
            res = None
        return res

    try:
        res = run_forthic()
        result = jsonify({'message': 'OK', 'result': res})
    except RuntimeError as e:
        result = jsonify(str(e))
        result.status_code = 400
    except Exception as e:
        result = jsonify(str(e))
        result.status_code = 500
    return result


@app.route('/update_password_form/<group>/<app>/<field>/')
@app.route('/update_password_form/<group>/<app>/<field>/<path:rest>')
def update_password_form(group, app, field, rest=None):
    return render_template(
        'update_password_form.html', group=group, app=app, field=field, rest=rest
    )

@app.route('/update_password', methods=['POST'])
def update_password():
    group = request.form['group']
    app = request.form['app']
    rest = request.form['rest']
    field = request.form['field']
    creds.store_password_creds(
        field,
        request.form['host'],
        request.form['username'],
        request.form['password'],
    )
    return redirect(url_for('forthic_app', group=group, app=app, rest=rest))


@app.route('/update_app_creds_form/<group>/<app>/<field>')
@app.route('/update_app_creds_form/<group>/<app>/<field>/<path:rest>')
def update_app_creds_form(group, app, field, rest=None):
    return render_template(
        'update_app_creds_form.html', group=group, app=app, field=field, rest=rest
    )

@app.route('/update_app_creds', methods=['POST'])
def update_app_creds():
    group = request.form['group']
    app = request.form['app']
    field = request.form['field']
    rest = request.form['rest']
    creds.store_app_creds(
        field, request.form['client_id'], request.form['client_secret']
    )
    return redirect(url_for('forthic_app', group=group, app=app, rest=rest))


@app.route('/update_google_oauth_token')
def update_google_oauth_token():
    token = get_google_token(creds, request.args['code'])
    creds.store_oauth_token('GOOGLE_TOKEN', token)
    group = session['group']
    app = session['app']
    rest = session["rest"]
    return redirect(url_for('forthic_app', group=group, app=app, rest=rest))


# -----------------------------------------------------------------------------
# Helpers


def read_file(filename):
    res = ''
    if os.path.exists(filename):
        with open(filename) as f:
            res = f.read()
    return res


def get_main_forthic(app_dir):
    main_forthic_filename = f'{app_dir}/main.forthic'
    result = read_file(main_forthic_filename)
    return result


def get_forthic_version(app_dir):
    """Looks for a config.json file and returns the forthic_version field. Defaults to 'v3'"""
    DEFAULT_VERSION = "v3"
    file_path = f"{app_dir}/config.json"
    if not os.path.exists(file_path):
        return DEFAULT_VERSION

    with open(file_path) as f:
        contents = f.read()
    config = json.loads(contents)
    forthic_version = config.get("forthic_version")
    if not forthic_version:
        forthic_version = DEFAULT_VERSION
    result = forthic_version
    return result


def get_interp(app_dir):
    forthic_version = get_forthic_version(app_dir)
    if (forthic_version == "v2"):
        return interp_v2.get_interp(app_dir)
    elif (forthic_version == "v3"):
        return interp_v3.get_interp(app_dir)
    else:
        raise UnknownInterpreterVersion(app_dir, forthic_version)


def get_forthic_react_template(forthic_react):
    result = ""
    if forthic_react.version == "v1":
        result = 'react/react-app/v1/main.html'
    else:
        raise RuntimeError(f"Unknown ForthicReact version: {forthic_react.version}")
    return result


def get_google_auth_url():
    app_creds = creds.get_app_creds('GOOGLE_APP')
    client_id = app_creds['client_id']
    redirect_uri = 'http://localhost:8000/update_google_oauth_token'
    scope = creds.get_oauth_cfg('GOOGLE_OAUTH_SCOPES')
    oauth = OAuth2Session(client_id, redirect_uri=redirect_uri, scope=scope)
    result, _ = oauth.authorization_url(
        'https://accounts.google.com/o/oauth2/v2/auth',
        access_type='offline',
        prompt='consent',
    )
    return result


def get_google_token(creds, code):
    app_creds = creds.get_app_creds('GOOGLE_APP')
    client_id = app_creds['client_id']
    client_secret = app_creds['client_secret']
    redirect_uri = 'http://localhost:8000/update_google_oauth_token'
    oauth = OAuth2Session(client_id, redirect_uri=redirect_uri)
    result = oauth.fetch_token(
        'https://oauth2.googleapis.com/token',
        code=code,
        client_secret=client_secret,
    )
    return result


def get_forthic_app_screens(app_dir):
    screens_dir = f"{app_dir}/screens"
    if not os.path.isdir(screens_dir):
        return []

    result = []
    with os.scandir(screens_dir) as entries:
        for entry in entries:
            match = re.match("(.+)\.forthic", entry.name)
            if entry.is_file() and match:
                with open(f"{screens_dir}/{entry.name}") as f:
                    screen_forthic = f.read()
                    screen_name = match.group(1)
                    result.append([screen_name, screen_forthic])
    return result


def get_app_forthic(app_dir):
    forthic_file = f"{app_dir}/main.forthic"
    with open(forthic_file) as f:
        result = f.read()
    return result


def run_app_forthic(interp, app_dir):
    """Runs the forthic in the app dir
    """
    # Store screens for this app in the app module
    forthic_screens = get_forthic_app_screens(app_dir)
    for s in forthic_screens:
        interp.app_module.set_screen(s[0], s[1])

    # Run the app_forthic and MAIN-PAGE
    interp.run(get_app_forthic(app_dir))
    return
