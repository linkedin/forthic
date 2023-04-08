# Examples
Because Forthic apps often pull data from 3rd party sources like gsheets and Jira, it's natural for them to run as web apps -- this makes it possible to implement the OAuth flows.

>These examples show how to incorporate a Forthic interpreter inside of a web app, but this is only for illustration >and not meant for production environments.


## Example: Using the Cache
The `coding-forthic/cache-example` stores an array of strings in the cache and then reads them out again.
* `make`
* Go to [http://localhost:8000/coding-forthic/cache-example/](http://localhost:8000/coding-forthic/cache-example/)


## Example: Read Google Sheets
This example pulls live gsheet data, so it requires a bit of setup to authorize the API calls
to Google.

### Setup
If you already have a Google OAuth client, all you'll need is your **client ID** and **secret**. Otherwise, you'll
need to generate new ones by registering an OAuth client with Google:
1. Create a new Project at the [Google Developer's Console](https://console.developers.google.com/)
1. In the sidebar, click on "OAuth consent screen" and select "Internal" as the user type
1. In the sidebar, click on "Enabled APIs & services" and enable the Gsheets API
1. In the sidebar, click on "Credentials" and create a new "OAuth 2.0 Client ID" noting the client ID and secret
    * Choose "Web application" from Application Type
    * Under Authorized redirect URIs add `http://localhost:8000/update_google_oauth_token`

### Configuration
1. Open the `main.forthic` file in `server/apps/coding-forthic/gsheet-example`
1. Change the `GSHEET-URL` to be the URL of a gsheet you can read
1. Save the file

### Run the example
* `make`
* Go to [http://localhost:8000/coding-forthic/gsheet-example/](http://localhost:8000/coding-forthic/gsheet-example/)

> If you haven't specified your client ID and secret, you will be redirected to a form where you can enter these.
> The values are stored encrypted in a `server/.secrets` file on your computer.

> If you haven't authorized your Google OAuth client to make calls on your behalf, you will be redirected to
> an OAuth Consent form where you can grant permission.

If everything works, you'll see the rows of your gsheet rendered into a webpage


## Example: Read Jira Data
In order to run this example, you'll need to get an apitoken from your Jira instance. Here are the steps:

1. From your Jira instance, click on your account icon in the right side of the menubar
1. Select "Manage account"
1. Click on the "Security" tab
1. Under "API Token", click on "Create and manage API tokens"
1. Click "Create API token" and make a note of it (you'll need it when you run the example for the first time)

### Run the example

* `make`
* Go to [http://localhost:8000/coding-forthic/jira-example/](http://localhost:8000/coding-forthic/jira-example/)

The first time you run this example, you'll be asked to fill out a Jira **host**, **username** and **API token**. After you submit the form, these will be encrypted and stored in `./app/examples/.secrets`.

The example will render a list of incomplete Jira tickets updated over the past 3 days.
