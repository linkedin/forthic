# intake-branched
This is an example of a branched intake form. You can run this out of the box because the
`.cache` file contains config information stored by running `REFRESH-CONFIGS!`. The source spreadsheet tabs
are [intake-branched - Start.tsv](./intake-branched%20-%20Start.tsv),  [intake-branched - Continue.tsv](./intake-branched%20-%20Continue.tsv), and [intake-branched - Self-service.tsv](./intake-branched%20-%20Self-service.tsv)

## Configuring for your own case
To run this on your own, make sure you've set up Forthic to [work with Jira](https://www.youtube.com/watch?v=sK4_8esQttw)
and [with Google Sheets](https://www.youtube.com/watch?v=BPg8RlQfUFE). Then you can do the following:

1. Update `CONFIG-URL` to point to a new Google sheet
2. Change `FORM-ADMINS` to include your user info (for Jira Cloud, this will be the email you use for your instance)
3. Update `FORM-CONFIGS` to reflect your Jira Project and Issue Type
4. Load the example and click on the "Refresh Configs" button. This will ensure that config tabs exist for all of your steps in your Google spreadsheet
5. Import [intake-branched - Start.tsv](./intake-branched%20-%20Start.tsv),  [intake-branched - Continue.tsv](./intake-branched%20-%20Continue.tsv), and [intake-branched - Self-service.tsv](./intake-branched%20-%20Self-service.tsv) and click "Refresh Configs" again
