import json
from forthic.modules.jira_module import JiraContext


class ServerResponse:
    def __init__(self, string, status_code=200):
        self.json_string = string
        self.status_code = status_code
        self.text = ""
        self.ok = status_code < 300

    def json(self):
        result = json.loads(self.json_string)
        return result


class JiraTestContext(JiraContext):
    def get_host(self):
        return "http://testcontext"

    def requests_get(self, api_url, session=None):
        result = ServerResponse("null")
        if api_url == "/rest/api/2/field":
            result = ServerResponse(REST_API_2_FIELD_RESPONSE)
        elif api_url == "/rest/api/2/issue/SAMPLE-101/votes":
            result = ServerResponse(VOTE_RESPONSE)
        elif (
            api_url
            == "/rest/api/2/issue/SAMPLE-101?expand=changelog&fields=customfield_10460,created"
        ):
            result = ServerResponse(CHANGELOG_RESPONSE)
        else:
            raise Exception(f"Unknown route: {api_url}")
        return result

    def requests_post(self, api_url, json=None, session=None):
        result = ServerResponse("null")
        if api_url == "/rest/api/2/search":
            if json["jql"] == "assignee=testuser and resolution is null" and json[
                "fields"
            ] == ["summary", "assignee"]:
                result = ServerResponse(SEARCH_RESPONSE1)
        elif api_url == "/rest/api/2/issue":
            result = ServerResponse('{"key": "SAMPLE-12345"}', 201)
        elif api_url == "/rest/api/2/issue/SAMPLE-1234/watchers":
            result = ServerResponse("null", 204)
        elif api_url == "/rest/api/2/issueLink":
            result = ServerResponse("null", 201)
        else:
            raise Exception(f"Unknown route: {api_url}")
        return result

    def requests_put(self, api_url, json=None, session=None):
        result = ServerResponse("null")
        if api_url == "/rest/api/2/issue/SAMPLE-1234":
            result = ServerResponse("null", 204)
        else:
            raise Exception(f"Unknown route: {api_url}")
        return result


REST_API_2_FIELD_RESPONSE = """
[{"id":"issuekey","name":"Key","custom":false,"orderable":false,"navigable":true,"searchable":false,
"clauseNames":["id","issue","issuekey","key"]},
{"id":"assignee","name":"Assignee","custom":false,"orderable":true,"navigable":true,"searchable":true,
"clauseNames":["assignee"],"schema":{"type":"user","system":"assignee"}},
{"id":"summary","name":"Summary","custom":false,"orderable":true,"navigable":true,"searchable":true,
"clauseNames":["summary"],"schema":{"type":"string","system":"summary"}},
{"id":"project","name":"Project","custom":false,"orderable":false,"navigable":true,"searchable":true,
"clauseNames":["project"],"schema":{"type":"project","system":"project"}},
{"id":"reporter","name":"Reporter","custom":false,"orderable":true,"navigable":true,"searchable":true,
"clauseNames":["reporter"],"schema":{"type":"user","system":"reporter"}},
{"id":"issuetype","name":"Issue Type","custom":false,"orderable":true,"navigable":true,"searchable":true,
"clauseNames":["issuetype","type"],"schema":{"type":"issuetype","system":"issuetype"}},
{"id":"customfield_10460","name":"Risk_Factor","custom":true,"orderable":true,"navigable":true,"searchable":true,
"clauseNames":["cf[10460]","Risk_Factor"],"schema":{"type":"option","custom":"com.atlassian.jira.plugin.system.customfieldtypes:select","customId":10460}},
{"id":"timespent","name":"Time Spent","custom":false,"orderable":false,"navigable":true,"searchable":false,
"clauseNames":["timespent"],"schema":{"type":"number","system":"timespent"}}]
"""

SEARCH_RESPONSE1 = """
{
    "expand": "schema,names",
    "startAt": 0,
    "maxResults": 200,
    "total": 2,
    "issues": [
        {
            "expand": "operations,versionedRepresentations,editmeta,changelog,renderedFields",
            "id": "15791174",
            "self": "https://testcontext/rest/api/2/issue/15791174",
            "key": "SAMPLE-1234",
            "fields": {
                "summary": "Forthic ask",
                "assignee": {
                    "self": "https://testcontext/rest/api/2/user?username=testuser",
                    "name": "testuser",
                    "key": "testuser",
                    "emailAddress": "testuser@testcontext",
                    "avatarUrls": {
                        "48x48": "https://testcontext/secure/useravatar?avatarId=10172",
                        "24x24": "https://testcontext/secure/useravatar?size=small&avatarId=10172",
                        "16x16": "https://testcontext/secure/useravatar?size=xsmall&avatarId=10172",
                        "32x32": "https://testcontext/secure/useravatar?size=medium&avatarId=10172"
                    },
                    "displayName": "Test User",
                    "active": true,
                    "timeZone": "America/Los_Angeles"
                }
            }
        },
        {
            "expand": "operations,versionedRepresentations,editmeta,changelog,renderedFields",
            "id": "15752784",
            "self": "https://testcontext/rest/api/2/issue/15752784",
            "key": "SAMPLE-1235",
            "fields": {
                "summary": "Forthic report",
                "assignee": {
                    "self": "https://testcontext/rest/api/2/user?username=testuser",
                    "name": "testuser",
                    "key": "testuser",
                    "emailAddress": "testuser@testcontext",
                    "avatarUrls": {
                        "48x48": "https://testcontext/secure/useravatar?avatarId=10172",
                        "24x24": "https://testcontext/secure/useravatar?size=small&avatarId=10172",
                        "16x16": "https://testcontext/secure/useravatar?size=xsmall&avatarId=10172",
                        "32x32": "https://testcontext/secure/useravatar?size=medium&avatarId=10172"
                    },
                    "displayName": "Test User",
                    "active": true,
                    "timeZone": "America/Los_Angeles"
                }
            }
        }]
}
"""

VOTE_RESPONSE = """
{
    "self": "https://testcontext/rest/api/2/issue/SAMPLE-101/votes",
    "votes": 2,
    "hasVoted": false,
    "voters": [
        {
            "self": "https://testcontext/rest/api/2/user?username=user1",
            "key": "user1",
            "name": "user1",
            "avatarUrls": {
                "48x48": "https://testcontext/secure/useravatar?ownerId=user1&avatarId=14334",
                "24x24": "https://testcontext/secure/useravatar?size=small&ownerId=user1&avatarId=14334",
                "16x16": "https://testcontext/secure/useravatar?size=xsmall&ownerId=user1&avatarId=14334",
                "32x32": "https://testcontext/secure/useravatar?size=medium&ownerId=user1&avatarId=14334"
            },
            "displayName": "User User1",
            "active": false
        },
        {
            "self": "https://testcontext/rest/api/2/user?username=user2",
            "key": "user2",
            "name": "user2",
            "avatarUrls": {
                "48x48": "https://testcontext/secure/useravatar?ownerId=user2&avatarId=13788",
                "24x24": "https://testcontext/secure/useravatar?size=small&ownerId=user2&avatarId=13788",
                "16x16": "https://testcontext/secure/useravatar?size=xsmall&ownerId=user2&avatarId=13788",
                "32x32": "https://testcontext/secure/useravatar?size=medium&ownerId=user2&avatarId=13788"
            },
            "displayName": "User User2",
            "active": false
        }
    ]
}"""

CHANGELOG_RESPONSE = """
{
    "expand": "renderedFields,names,schema,operations,editmeta,changelog,versionedRepresentations",
    "id": "15117861",
    "self": "https://testcontext/rest/api/2/issue/15117861",
    "key": "SAMPLE-10112",
    "fields": {
        "customfield_10460": {
            "self": "https://testcontext/rest/api/2/customFieldOption/32077",
            "value": "Red",
            "id": "32077"
        },
        "created": "2020-07-25T01:36:24.000+0000"
    },
    "changelog": {
        "startAt": 0,
        "maxResults": 3,
        "total": 3,
        "histories": [
            {
                "id": "82031758",
                "author": {
                    "self": "https://testcontext/rest/api/2/user?username=user2",
                    "name": "user2",
                    "key": "user2",
                    "emailAddress": "user2@linkedin.com",
                    "avatarUrls": {
                        "48x48": "https://testcontext/secure/useravatar?avatarId=10172",
                        "24x24": "https://testcontext/secure/useravatar?size=small&avatarId=10172",
                        "16x16": "https://testcontext/secure/useravatar?size=xsmall&avatarId=10172",
                        "32x32": "https://testcontext/secure/useravatar?size=medium&avatarId=10172"
                    },
                    "displayName": "User User2",
                    "active": true,
                    "timeZone": "America/Los_Angeles"
                },
                "created": "2020-07-25T01:36:25.000+0000",
                "items": [
                    {
                        "field": "Link",
                        "fieldtype": "jira",
                        "from": null,
                        "fromString": null,
                        "to": "SAMPLE-9465",
                        "toString": "This issue cloned from SAMPLE-9465"
                    }
                ]
            },
            {
                "id": "82031773",
                "author": {
                    "self": "https://testcontext/rest/api/2/user?username=user2",
                    "name": "user2",
                    "key": "user2",
                    "emailAddress": "user2@linkedin.com",
                    "avatarUrls": {
                        "48x48": "https://testcontext/secure/useravatar?avatarId=10172",
                        "24x24": "https://testcontext/secure/useravatar?size=small&avatarId=10172",
                        "16x16": "https://testcontext/secure/useravatar?size=xsmall&avatarId=10172",
                        "32x32": "https://testcontext/secure/useravatar?size=medium&avatarId=10172"
                    },
                    "displayName": "User User2",
                    "active": true,
                    "timeZone": "America/Los_Angeles"
                },
                "created": "2020-07-25T01:38:46.000+0000",
                "items": [
                    {
                        "field": "Risk_Factor",
                        "fieldtype": "custom",
                        "from": "32078",
                        "fromString": "Blue",
                        "to": "32075",
                        "toString": "Green"
                    },
                    {
                        "field": "Target start",
                        "fieldtype": "custom",
                        "from": "2020-04-01",
                        "fromString": "1/Apr/20",
                        "to": "2020-06-22",
                        "toString": "22/Jun/20"
                    },
                    {
                        "field": "assignee",
                        "fieldtype": "jira",
                        "from": "user3",
                        "fromString": "User User3",
                        "to": "user4",
                        "toString": "User User4"
                    },
                    {
                        "field": "duedate",
                        "fieldtype": "jira",
                        "from": "2020-05-21",
                        "fromString": "2020-05-21 00:00:00.0",
                        "to": "2020-08-28",
                        "toString": "2020-08-28 00:00:00.0"
                    },
                    {
                        "field": "labels",
                        "fieldtype": "jira",
                        "from": null,
                        "fromString": "fy20q4",
                        "to": null,
                        "toString": "fy20q4 fy21q1"
                    }
                ]
            },
            {
                "id": "82031777",
                "author": {
                    "self": "https://testcontext/rest/api/2/user?username=user2",
                    "name": "user2",
                    "key": "user2",
                    "emailAddress": "user2@linkedin.com",
                    "avatarUrls": {
                        "48x48": "https://testcontext/secure/useravatar?avatarId=10172",
                        "24x24": "https://testcontext/secure/useravatar?size=small&avatarId=10172",
                        "16x16": "https://testcontext/secure/useravatar?size=xsmall&avatarId=10172",
                        "32x32": "https://testcontext/secure/useravatar?size=medium&avatarId=10172"
                    },
                    "displayName": "User User2",
                    "active": true,
                    "timeZone": "America/Los_Angeles"
                },
                "created": "2020-08-15T01:39:05.000+0000",
                "items": [
                    {
                        "field": "Link",
                        "fieldtype": "jira",
                        "from": null,
                        "fromString": null,
                        "to": "SAMPLE-10113",
                        "toString": "This issue cloned to SAMPLE-10113"
                    },
                    {
                        "field": "Risk_Factor",
                        "fieldtype": "custom",
                        "from": "32078",
                        "fromString": "Green",
                        "to": "32075",
                        "toString": "Yellow"
                    }
                ]
            }
    ]}
}
"""
