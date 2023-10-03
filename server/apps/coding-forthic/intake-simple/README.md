# intake-simple
This is an example of a single step intake form. You can run this out of the box because the
`.cache` file contains config information stored by running `REFRESH-CONFIGS!`. The source spreadsheet
is in the [intake-simple - Simple.tsv](./intake-simple%20-%20Simple.tsv) file.

## Configuring for your own case
To run this on your own, make sure you've set up Forthic to [work with Jira](https://www.youtube.com/watch?v=sK4_8esQttw)
and [with Google Sheets](https://www.youtube.com/watch?v=BPg8RlQfUFE). Then you can do the following:

1. Update `CONFIG-URL` to point to a new Google sheet
2. Change `FORM-ADMINS` to include your user info (for Jira Cloud, this will be the email you use for your instance)
3. Update `FORM-CONFIGS` to reflect your Jira Project and Issue Type
4. Load the example and click on the "Refresh Configs" button. This will ensure that a config tab exists in your Google sheet
5. Import the [intake-simple - Simple.tsv](./intake-simple%20-%20Simple.tsv) file into your config tab and click "Refresh Configs" again
