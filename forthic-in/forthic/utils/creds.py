import os
import json
from cryptography.fernet import Fernet


class MissingSecretsFile(RuntimeError):
    pass


class MissingPasswordCreds(RuntimeError):
    def __init__(self, field):
        super().__init__(f'Missing field: {field}')
        self.field = field


class MissingAppCreds(RuntimeError):
    def __init__(self, field):
        super().__init__(f'Missing field: {field}')
        self.field = field


class MissingOAuthToken(RuntimeError):
    def __init__(self, field):
        super().__init__(f'Missing field: {field}')
        self.field = field


class MissingAlationCreds(RuntimeError):
    def __init__(self, field, host):
        super().__init__(f'Missing field: {field}')
        self.field = field
        self.host = host


SECRETS_FILENAME = '.secrets'
KEY_FILENAME = '.key'


class Creds:
    """Manages credentials by storing them encrypted on disk.
    This is suitable for running examples and doing local development but not for production.
    """

    def __init__(self, directory):
        self.directory = directory

    def get_password_creds(self, field):
        if not self.does_secrets_file_exist():
            raise MissingSecretsFile()

        result = self.get_creds(field)
        if not result:
            raise MissingPasswordCreds(field)

        decrypted_password = self.decrypt_string(result['password'])
        result['password'] = decrypted_password
        return result

    def store_password_creds(self, field, host, username, password):
        secrets = self.load_json(self.get_secrets_filepath())
        encrypted_password = self.encrypt_string(password)
        record = {
            'host': host,
            'username': username,
            'password': encrypted_password,
        }
        secrets[field] = record
        self.store_secrets(secrets)

    def get_app_creds(self, field):
        if not self.does_secrets_file_exist():
            raise MissingSecretsFile()

        result = self.get_creds(field)
        if not result:
            raise MissingAppCreds(field)

        decrypted_secret = self.decrypt_string(result['client_secret'])
        result['client_secret'] = decrypted_secret
        return result

    def store_app_creds(self, field, client_id, client_secret):
        secrets = self.load_json(self.get_secrets_filepath())
        encrypted_client_secret = self.encrypt_string(client_secret)
        record = {
            'client_id': client_id,
            'client_secret': encrypted_client_secret,
        }
        secrets[field] = record
        self.store_secrets(secrets)

    def get_oauth_token(self, field):
        if not self.does_secrets_file_exist():
            raise MissingSecretsFile()

        encrypted_token = self.get_creds(field)
        if not encrypted_token:
            raise MissingOAuthToken(field)

        result = json.loads(self.decrypt_string(encrypted_token))
        return result

    def store_oauth_token(self, field, token):
        if not self.does_secrets_file_exist():
            raise MissingSecretsFile()

        json_string = json.dumps(token)
        encrypted_string = self.encrypt_string(json_string)

        secrets = self.load_json(self.get_secrets_filepath())
        secrets[field] = encrypted_string
        self.store_secrets(secrets)

    def get_oauth_cfg(self, field):
        oauth_cfg = self.load_json(f'{self.directory}/oauth_cfg.json')
        result = oauth_cfg[field]
        return result

    def get_alation_creds(self, field, host=None):
        """If `host` is specified, it will be used in the form to update Alation creds"""
        if not self.does_secrets_file_exist():
            raise MissingSecretsFile()

        result = self.get_creds(field)
        if not result:
            raise MissingAlationCreds(field, host)

        decrypted_refresh_token = self.decrypt_string(result['refresh_token'])
        result['refresh_token'] = decrypted_refresh_token
        return result

    def store_alation_creds(self, field, host, user_id, refresh_token):
        secrets = self.load_json(self.get_secrets_filepath())
        encrypted_refresh_token = self.encrypt_string(refresh_token)
        record = {
            'host': host,
            'user_id': user_id,
            'refresh_token': encrypted_refresh_token,
        }
        secrets[field] = record
        self.store_secrets(secrets)

    def delete_creds(self, field):
        secrets = self.load_json(self.get_secrets_filepath())
        del secrets[field]
        self.store_secrets(secrets)

    # -------------------------
    # Helpers

    def get_key_filepath(self):
        return f'{self.directory}/{KEY_FILENAME}'

    def get_secrets_filepath(self):
        return f'{self.directory}/{SECRETS_FILENAME}'

    def does_key_file_exist(self):
        result = os.path.isfile(self.get_key_filepath())
        return result

    def does_secrets_file_exist(self):
        result = os.path.isfile(self.get_secrets_filepath())
        return result

    def ensure_key(self):
        if not self.does_key_file_exist():
            key = Fernet.generate_key()
            with open(self.get_key_filepath(), 'wb') as f:
                f.write(key)
        with open(self.get_key_filepath(), 'rb') as f:
            result = f.read()
        return result

    def ensure_secrets_file(self):
        if not self.does_secrets_file_exist():
            with open(self.get_secrets_filepath(), 'w') as f:
                f.write(json.dumps({}))

    def get_key(self):
        with open(self.get_key_filepath(), 'rb') as f:
            result = f.read()
        return result

    def encrypt_string(self, string):
        key = self.get_key()
        fernet = Fernet(key)
        message = string.encode()
        result = fernet.encrypt(message).decode()
        return result

    def decrypt_string(self, string):
        key = self.get_key()
        if not string:
            return string
        fernet = Fernet(key)
        message = string.encode()
        result = fernet.decrypt(message).decode()
        return result

    def get_creds(self, field):
        secrets = self.load_json(self.get_secrets_filepath())
        result = secrets.get(field)
        return result

    def load_json(self, filename):
        with open(filename) as f:
            result = json.loads(f.read())
        return result

    def store_secrets(self, secrets):
        with open(self.get_secrets_filepath(), 'w') as f:
            f.write(json.dumps(secrets, indent=4, separators=(',', ': ')))
