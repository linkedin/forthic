import json


def load_json(filename):
    with open(filename) as f:
        result = json.loads(f.read())
    return result


def get_creds(field):
    secrets = load_json('./apps/.secrets')
    result = secrets.get(field)
    return result


def store_token(field, token):
    secrets = load_json('./apps/.secrets')
    secrets[field] = token
    with open('./apps/.secrets', 'w') as f:
        f.write(json.dumps(secrets, indent=4, separators=(',', ': ')))
