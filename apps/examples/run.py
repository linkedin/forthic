import os
import re
from requests_oauthlib import OAuth2Session
from flask import Flask, render_template, request, redirect, url_for, session
import markdown

from forthic.interpreter import Interpreter
import forthic.modules.jira_module as jira_module
import forthic.modules.gsheet_module as gsheet_module
import forthic.modules.excel_module as excel_module
from forthic.modules.cache_module import CacheModule
from forthic.modules.jinja_module import JinjaModule
from forthic.modules.html_module import HtmlModule
from forthic.modules.org_module import OrgModule
from forthic.modules.confluence_module import ConfluenceModule
from forthic.utils.creds import (
    Creds,
    MissingSecretsFile,
    MissingPasswordCreds,
    MissingAppCreds,
    MissingOAuthToken,
)
from forthic.utils.errors import UnauthorizedError
from example_contexts_module import ExampleContextsModule

# Allow us to use http locally
os.environ['OAUTHLIB_INSECURE_TRANSPORT'] = '1'

SECRETS_DIR = '..'

creds = Creds(SECRETS_DIR)
creds.ensure_key()

app = Flask(__name__)
app.config['TEMPLATES_AUTO_RELOAD'] = True
app.secret_key = creds.get_key()


@app.after_request
def add_header(response):
    response.cache_control.max_age = 0
    return response


# ===== Routes =================================================================
@app.route('/')
def main_page():
    def get_example_dirs():
        directory = '.'
        files = os.listdir(directory)
        directories = [f for f in files if os.path.isdir(f'{directory}/{f}')]
        res = [d for d in directories if re.match('ex_.*', d)]
        return res

    def get_description(dir_name):
        desc_filename = f'./{dir_name}/description.md'
        if os.path.exists(desc_filename):
            with open(desc_filename) as f:
                contents = f.read()
        else:
            contents = '`<No description>`'

        res = markdown.markdown(contents)
        return res

    def get_example_infos():
        dirs = get_example_dirs()
        res = []
        for d in dirs:
            info = {'name': d, 'description': get_description(d)}
            res.append(info)
        return res

    ex_infos = get_example_infos()
    return render_template('main.html', ex_infos=ex_infos)


@app.route('/examples/<example>')
def example(example):
    try:
        directory = '.'
        example_dir = f'{directory}/{example}'
        interp = get_interp(example_dir)

        register_example_screens(interp, example_dir)
        example_forthic = get_example_forthic(example_dir)
        interp.run(example_forthic)

        def get_html_output():
            interp.run('MAIN-PAGE')
            res = interp.stack_pop()
            return res

        overview = markdown.markdown(read_file(f'{example_dir}/overview.md'))
        html_output = get_html_output()
        result = render_template(
            'example.html',
            example=example,
            overview=overview,
            example_forthic=example_forthic,
            html_output=html_output,
        )
    except MissingSecretsFile:
        creds.ensure_secrets_file()
        return redirect(url_for('example', example=example))
    except MissingPasswordCreds as e:
        return redirect(
            url_for('update_password_form', example=example, field=e.field)
        )
    except UnauthorizedError as e:
        return redirect(
            url_for('update_password_form', example=example, field=e.field)
        )
    except MissingAppCreds as e:
        return redirect(
            url_for('update_app_creds_form', example=example, field=e.field)
        )
    except MissingOAuthToken as e:
        session['example'] = example
        if e.field == 'GOOGLE_TOKEN':
            return redirect(get_google_auth_url())
        elif e.field == 'MSGRAPH_TOKEN':
            pass
        else:
            raise RuntimeError(f'Unknown OAuth token type: {e.field}')
    return result


@app.route('/update_password_form/<example>/<field>')
def update_password_form(example, field):
    return render_template(
        'update_password_form.html', example=example, field=field
    )


@app.route('/update_app_creds_form/<example>/<field>')
def update_app_creds_form(example, field):
    return render_template(
        'update_app_creds_form.html', example=example, field=field
    )


@app.route('/update_password', methods=['POST'])
def update_password():
    example = request.form['example']
    field = request.form['field']
    creds.store_password_creds(
        field,
        request.form['host'],
        request.form['username'],
        request.form['password'],
    )
    return redirect(url_for('example', example=example))


@app.route('/update_app_creds', methods=['POST'])
def update_app_creds():
    example = request.form['example']
    field = request.form['field']
    creds.store_app_creds(
        field, request.form['client_id'], request.form['client_secret']
    )
    return redirect(url_for('example', example=example))


@app.route('/update_google_oauth_token')
def update_google_oauth_token():
    token = get_google_token(creds, request.args['code'])
    creds.store_oauth_token('GOOGLE_TOKEN', token)
    example = session['example']
    return redirect(url_for('example', example=example))


# -----------------------------------------------------------------------------
# Helpers
def register_example_screens(interp, example_dir):
    # NOTE: This is here as a placeholder when there are example screens to be loaded
    # Store screens for this app in the app module
    # forthic_screens = get_forthic_app_screens(app_dir)
    # for s in forthic_screens:
    #     interp.app_module.set_screen(s[0], s[1])
    pass


def read_file(filename):
    res = ''
    if os.path.exists(filename):
        with open(filename) as f:
            res = f.read()
    return res


def get_example_forthic(example_dir):
    main_forthic_filename = f'{example_dir}/main.forthic'
    result = read_file(main_forthic_filename)
    return result


def get_interp(app_dir):
    def configure_cache_module(interp):
        interp.register_module(CacheModule)
        interp.run(f"['cache'] USE-MODULES '{app_dir}' cache.CWD!")

    def configure_html_module(interp):
        interp.register_module(HtmlModule)
        js_path = '/static/forthic/forthic-js'
        interp.run(f"['html'] USE-MODULES '{js_path}' html.JS-PATH!")

    interp = Interpreter()
    interp.dev_mode = True

    configure_html_module(interp)
    configure_cache_module(interp)

    interp.register_module(gsheet_module.GsheetModule)
    interp.register_module(excel_module.ExcelModule)
    interp.register_module(jira_module.JiraModule)
    interp.register_module(JinjaModule)
    interp.register_module(ConfluenceModule)
    interp.register_module(ExampleContextsModule)
    interp.register_module(OrgModule)
    return interp


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
