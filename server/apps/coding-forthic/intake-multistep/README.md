# intake-multistep
This is an example of a multistep intake form. You can run this out of the box because the
`.cache` file contains config information stored by running `REFRESH-CONFIGS!`. The source spreadsheet tabs
are [intake-multistep - Multistep 1.tsv](./intake-multistep%20-%20Multistep%201.tsv) and [intake-multistep - Multistep 2.tsv](./intake-multistep%20-%20Multistep%202.tsv)

## Configuring for your own case
To run this on your own, make sure you've set up Forthic to [work with Jira](https://www.youtube.com/watch?v=sK4_8esQttw)
and [with Google Sheets](https://www.youtube.com/watch?v=BPg8RlQfUFE). Then you can do the following:

1. Update `CONFIG-URL` to point to a new Google sheet
2. Change `FORM-ADMINS` to include your user info (for Jira Cloud, this will be the email you use for your instance)
3. Update `FORM-CONFIGS` to reflect your Jira Project and Issue Type
4. Update the `step_tabs` field `FORM-CONFIGS` to reflect the number of steps in your form
5. Load the example and click on the "Refresh Configs" button. This will ensure that config tabs exist for all of your steps in your Google spreadsheet
6. Import [intake-multistep - Multistep 1.tsv](./intake-multistep%20-%20Multistep%201.tsv) and [intake-multistep - Multistep 2.tsv](./intake-multistep%20-%20Multistep%202.tsv) and click "Refresh Configs" again
