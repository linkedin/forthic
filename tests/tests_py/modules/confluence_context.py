import json
from forthic.modules.confluence_module import ConfluenceContext

class ServerResponse:
    def __init__(self, string, status_code=200):
        self.json_string = string
        self.status_code = status_code
        self.text = ""
    
    def json(self):
        result = json.loads(self.json_string)
        return result



class ConfluenceTestContext(ConfluenceContext):
    def __init__(self):
        self.page_created = False

    def get_host(self):
        return "http://testcontext"

    def requests_get(self, api_url):
        result = ServerResponse("null")
        if api_url == '/wiki/cf/rest/api/content?title=A+page+title&spaceKey=SPACE&expand=version':
            result = ServerResponse(PAGE_INFO_RESPONSE)
        elif not self.page_created and api_url == '/wiki/cf/rest/api/content?title=A+new+page+title&spaceKey=SPACE&expand=ancestors':
            result = ServerResponse(NO_PAGE_INFO_RESPONSE)
        elif self.page_created and api_url == '/wiki/cf/rest/api/content?title=A+new+page+title&spaceKey=SPACE&expand=ancestors':
            result = ServerResponse(PAGE_INFO_w_ANCESTORS_RESPONSE)
        elif api_url == '/wiki/cf/rest/api/content?title=A+parent+title&spaceKey=SPACE&expand=version':
            result = ServerResponse(PAGE_INFO_RESPONSE)
        elif api_url == '/wiki/cf/rest/api/content?title=A+new+page+title&spaceKey=SPACE&expand=version':
            result = ServerResponse(PAGE_INFO_RESPONSE)
        else:
            raise Exception(f"Unknown route: {api_url}")
        return result

    def requests_post(self, api_url, json=None):
        result = ServerResponse("null")
        if api_url == "/wiki/cf/rest/api/content":
            self.page_created = True
            result = ServerResponse(CREATE_PAGE_RESPONSE)
        else:
            raise Exception(f"Unknown route: {api_url}")
        return result

    def requests_put(self, api_url, json=None):
        result = ServerResponse("null")
        if api_url == '/wiki/cf/rest/api/content/1234':
            result = ServerResponse(UPDATE_PAGE_RESPONSE)
        else:
            raise Exception(f"Unknown route: {api_url}")
        return result


PAGE_INFO_RESPONSE = '''
{
    "results": [
        {
            "id": "1234",
            "type": "page",
            "status": "current",
            "title": "A page title",
            "version": {
                "by": {
                    "type": "known",
                    "username": "testuser",
                    "userKey": "2c9239b948dc82440148dc876925181a",
                    "profilePicture": {
                        "path": "/wiki/cf/images/icons/profilepics/default.svg",
                        "width": 48,
                        "height": 48,
                        "isDefault": true
                    },
                    "displayName": "Test User",
                    "_links": {
                        "self": "https://testcontext/wiki/cf/rest/api/user?key=2c9239b948dc82440148dc876925181a"
                    },
                    "_expandable": {
                        "status": ""
                    }
                },
                "when": "2020-10-23T16:54:50.000Z",
                "message": "",
                "number": 3,
                "minorEdit": false,
                "hidden": false,
                "_links": {
                    "self": "https://testcontext/wiki/cf/rest/experimental/content/1234/version/3"
                },
                "_expandable": {
                    "content": "/rest/api/content/1234"
                }
            },
            "extensions": {
                "position": "none"
            },
            "_links": {
                "webui": "/display/SPACE/A+page+title",
                "edit": "/pages/resumedraft.action?draftId=1234&draftShareId=0b59bcea-e6ea-44cc-a0b1-745f7d9e441d",
                "tinyui": "/x/chcmFw",
                "self": "https://testcontext/wiki/cf/rest/api/content/1234"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/1234/child",
                "restrictions": "/rest/api/content/1234/restriction/byOperation",
                "history": "/rest/api/content/1234/history",
                "ancestors": "",
                "body": "",
                "descendants": "/rest/api/content/1234/descendant",
                "space": "/rest/api/space/SPACE"
            }
        }
    ],
    "start": 0,
    "limit": 25,
    "size": 1,
    "_links": {
        "self": "https://testcontext/wiki/cf/rest/api/content?spaceKey=SPACE&expand=version&title=A%20page%20title",
        "base": "https://testcontext/wiki/cf",
        "context": "/wiki/cf"
    }
}
'''

NO_PAGE_INFO_RESPONSE ='''
{
    "results": [],
    "start": 0,
    "limit": 25,
    "size": 0,
    "_links": {
        "self": "https://testcontext/wiki/cf/rest/api/content?spaceKey=SPACE&expand=ancestors&title=A%20new%20page%20title",
        "base": "https://testcontext/wiki/cf",
        "context": "/wiki/cf"
    }
}
'''

PAGE_INFO_w_ANCESTORS_RESPONSE = '''
{
    "results": [
        {
            "id": "388386405",
            "type": "page",
            "status": "current",
            "title": "A new page title",
            "ancestors": [
                {
                    "id": "119239646",
                    "type": "page",
                    "status": "current",
                    "title": "Space Home",
                    "extensions": {
                        "position": "none"
                    },
                    "_links": {
                        "webui": "/display/SPACE/Space+Home",
                        "edit": "/pages/resumedraft.action?draftId=119239646",
                        "tinyui": "/x/3nMbBw",
                        "self": "https://testcontext/wiki/cf/rest/api/content/119239646"
                    },
                    "_expandable": {
                        "container": "/rest/api/space/SPACE",
                        "metadata": "",
                        "operations": "",
                        "children": "/rest/api/content/119239646/child",
                        "restrictions": "/rest/api/content/119239646/restriction/byOperation",
                        "history": "/rest/api/content/119239646/history",
                        "ancestors": "",
                        "body": "",
                        "version": "",
                        "descendants": "/rest/api/content/119239646/descendant",
                        "space": "/rest/api/space/SPACE"
                    }
                },
                {
                    "id": "148348821",
                    "type": "page",
                    "status": "current",
                    "title": "Project SPACE",
                    "extensions": {
                        "position": 85
                    },
                    "_links": {
                        "webui": "/display/SPACE/Project+SPACE",
                        "edit": "/pages/resumedraft.action?draftId=148348821",
                        "tinyui": "/x/lZ-XC",
                        "self": "https://testcontext/wiki/cf/rest/api/content/148348821"
                    },
                    "_expandable": {
                        "container": "/rest/api/space/SPACE",
                        "metadata": "",
                        "operations": "",
                        "children": "/rest/api/content/148348821/child",
                        "restrictions": "/rest/api/content/148348821/restriction/byOperation",
                        "history": "/rest/api/content/148348821/history",
                        "ancestors": "",
                        "body": "",
                        "version": "",
                        "descendants": "/rest/api/content/148348821/descendant",
                        "space": "/rest/api/space/SPACE"
                    }
                },
                {
                    "id": "148348824",
                    "type": "page",
                    "status": "current",
                    "title": "Forthic",
                    "extensions": {
                        "position": "none"
                    },
                    "_links": {
                        "webui": "/display/SPACE/Forthic",
                        "edit": "/pages/resumedraft.action?draftId=148348824",
                        "tinyui": "/x/mJ-XC",
                        "self": "https://testcontext/wiki/cf/rest/api/content/148348824"
                    },
                    "_expandable": {
                        "container": "/rest/api/space/SPACE",
                        "metadata": "",
                        "operations": "",
                        "children": "/rest/api/content/148348824/child",
                        "restrictions": "/rest/api/content/148348824/restriction/byOperation",
                        "history": "/rest/api/content/148348824/history",
                        "ancestors": "",
                        "body": "",
                        "version": "",
                        "descendants": "/rest/api/content/148348824/descendant",
                        "space": "/rest/api/space/SPACE"
                    }
                },
                {
                    "id": "248042769",
                    "type": "page",
                    "status": "current",
                    "title": "Forthic Framework",
                    "extensions": {
                        "position": "none"
                    },
                    "_links": {
                        "webui": "/display/SPACE/Forthic+Framework",
                        "edit": "/pages/resumedraft.action?draftId=248042769&draftShareId=e6dfb6e9-9a98-4e20-8afc-8fdfaa354ace",
                        "tinyui": "/x/EdXIDg",
                        "self": "https://testcontext/wiki/cf/rest/api/content/248042769"
                    },
                    "_expandable": {
                        "container": "/rest/api/space/SPACE",
                        "metadata": "",
                        "operations": "",
                        "children": "/rest/api/content/248042769/child",
                        "restrictions": "/rest/api/content/248042769/restriction/byOperation",
                        "history": "/rest/api/content/248042769/history",
                        "ancestors": "",
                        "body": "",
                        "version": "",
                        "descendants": "/rest/api/content/248042769/descendant",
                        "space": "/rest/api/space/SPACE"
                    }
                },
                {
                    "id": "261397943",
                    "type": "page",
                    "status": "current",
                    "title": "A parent title",
                    "extensions": {
                        "position": "none"
                    },
                    "_links": {
                        "webui": "/display/SPACE/Forthic+Testing",
                        "edit": "/pages/resumedraft.action?draftId=261397943&draftShareId=5ebe725d-0d69-46a5-bf3f-5cd01d7c17c7",
                        "tinyui": "/x/t52UDw",
                        "self": "https://testcontext/wiki/cf/rest/api/content/261397943"
                    },
                    "_expandable": {
                        "container": "/rest/api/space/SPACE",
                        "metadata": "",
                        "operations": "",
                        "children": "/rest/api/content/261397943/child",
                        "restrictions": "/rest/api/content/261397943/restriction/byOperation",
                        "history": "/rest/api/content/261397943/history",
                        "ancestors": "",
                        "body": "",
                        "version": "",
                        "descendants": "/rest/api/content/261397943/descendant",
                        "space": "/rest/api/space/SPACE"
                    }
                }
            ],
            "extensions": {
                "position": "none"
            },
            "_links": {
                "webui": "/display/SPACE/A+new+page+title",
                "edit": "/pages/resumedraft.action?draftId=388386405",
                "tinyui": "/x/ZU4mFw",
                "self": "https://testcontext/wiki/cf/rest/api/content/388386405"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/388386405/child",
                "restrictions": "/rest/api/content/388386405/restriction/byOperation",
                "history": "/rest/api/content/388386405/history",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/388386405/descendant",
                "space": "/rest/api/space/SPACE"
            }
        }
    ],
    "start": 0,
    "limit": 25,
    "size": 1,
    "_links": {
        "self": "https://testcontext/wiki/cf/rest/api/content?spaceKey=SPACE&expand=ancestors&title=A%20new%20page%20title",
        "base": "https://testcontext/wiki/cf",
        "context": "/wiki/cf"
    }
}
'''


CREATE_PAGE_RESPONSE='''
{
    "id": "388386403",
    "type": "page",
    "status": "current",
    "title": "A new page title",
    "space": {
        "id": 119963668,
        "key": "SPACE",
        "name": "Space",
        "type": "global",
        "_links": {
            "webui": "/display/SPACE",
            "self": "https://testcontext/wiki/cf/rest/api/space/SPACE"
        },
        "_expandable": {
            "metadata": "",
            "icon": "",
            "description": "",
            "homepage": "/rest/api/content/119239646"
        }
    },
    "history": {
        "latest": true,
        "createdBy": {
            "type": "known",
            "username": "SPACE-auto",
            "userKey": "2c9239b948dc82440148dc875dc709a1",
            "profilePicture": {
                "path": "/wiki/cf/images/icons/profilepics/default.svg",
                "width": 48,
                "height": 48,
                "isDefault": true
            },
            "displayName": "SPACE-auto",
            "_links": {
                "self": "https://testcontext/wiki/cf/rest/api/user?key=2c9239b948dc82440148dc875dc709a1"
            },
            "_expandable": {
                "status": ""
            }
        },
        "createdDate": "2020-10-30T15:48:59.311Z",
        "_links": {
            "self": "https://testcontext/wiki/cf/rest/api/content/388386403/history"
        },
        "_expandable": {
            "lastUpdated": "",
            "previousVersion": "",
            "contributors": "",
            "nextVersion": ""
        }
    },
    "version": {
        "by": {
            "type": "known",
            "username": "SPACE-auto",
            "userKey": "2c9239b948dc82440148dc875dc709a1",
            "profilePicture": {
                "path": "/wiki/cf/images/icons/profilepics/default.svg",
                "width": 48,
                "height": 48,
                "isDefault": true
            },
            "displayName": "SPACE-auto",
            "_links": {
                "self": "https://testcontext/wiki/cf/rest/api/user?key=2c9239b948dc82440148dc875dc709a1"
            },
            "_expandable": {
                "status": ""
            }
        },
        "when": "2020-10-30T15:48:59.311Z",
        "message": "",
        "number": 1,
        "minorEdit": false,
        "hidden": false,
        "_links": {
            "self": "https://testcontext/wiki/cf/rest/experimental/content/388386403/version/1"
        },
        "_expandable": {
            "content": "/rest/api/content/388386403"
        }
    },
    "ancestors": [
        {
            "id": "119239646",
            "type": "page",
            "status": "current",
            "title": "Space Home",
            "extensions": {
                "position": "none"
            },
            "_links": {
                "webui": "/display/SPACE/Space+Home",
                "edit": "/pages/resumedraft.action?draftId=119239646",
                "tinyui": "/x/3nMbBw",
                "self": "https://testcontext/wiki/cf/rest/api/content/119239646"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/119239646/child",
                "restrictions": "/rest/api/content/119239646/restriction/byOperation",
                "history": "/rest/api/content/119239646/history",
                "ancestors": "",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/119239646/descendant",
                "space": "/rest/api/space/SPACE"
            }
        },
        {
            "id": "148348821",
            "type": "page",
            "status": "current",
            "title": "Project SPACE",
            "extensions": {
                "position": 85
            },
            "_links": {
                "webui": "/display/SPACE/Project+SPACE",
                "edit": "/pages/resumedraft.action?draftId=148348821",
                "tinyui": "/x/lZ-XC",
                "self": "https://testcontext/wiki/cf/rest/api/content/148348821"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/148348821/child",
                "restrictions": "/rest/api/content/148348821/restriction/byOperation",
                "history": "/rest/api/content/148348821/history",
                "ancestors": "",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/148348821/descendant",
                "space": "/rest/api/space/SPACE"
            }
        },
        {
            "id": "148348824",
            "type": "page",
            "status": "current",
            "title": "Forthic",
            "extensions": {
                "position": "none"
            },
            "_links": {
                "webui": "/display/SPACE/Forthic",
                "edit": "/pages/resumedraft.action?draftId=148348824",
                "tinyui": "/x/mJ-XC",
                "self": "https://testcontext/wiki/cf/rest/api/content/148348824"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/148348824/child",
                "restrictions": "/rest/api/content/148348824/restriction/byOperation",
                "history": "/rest/api/content/148348824/history",
                "ancestors": "",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/148348824/descendant",
                "space": "/rest/api/space/SPACE"
            }
        },
        {
            "id": "248042769",
            "type": "page",
            "status": "current",
            "title": "Forthic Framework",
            "extensions": {
                "position": "none"
            },
            "_links": {
                "webui": "/display/SPACE/Forthic+Framework",
                "edit": "/pages/resumedraft.action?draftId=248042769&draftShareId=e6dfb6e9-9a98-4e20-8afc-8fdfaa354ace",
                "tinyui": "/x/EdXIDg",
                "self": "https://testcontext/wiki/cf/rest/api/content/248042769"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/248042769/child",
                "restrictions": "/rest/api/content/248042769/restriction/byOperation",
                "history": "/rest/api/content/248042769/history",
                "ancestors": "",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/248042769/descendant",
                "space": "/rest/api/space/SPACE"
            }
        },
        {
            "id": "261397943",
            "type": "page",
            "status": "current",
            "title": "A parent title",
            "extensions": {
                "position": "none"
            },
            "_links": {
                "webui": "/display/SPACE/Forthic+Testing",
                "edit": "/pages/resumedraft.action?draftId=261397943&draftShareId=5ebe725d-0d69-46a5-bf3f-5cd01d7c17c7",
                "tinyui": "/x/t52UDw",
                "self": "https://testcontext/wiki/cf/rest/api/content/261397943"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/261397943/child",
                "restrictions": "/rest/api/content/261397943/restriction/byOperation",
                "history": "/rest/api/content/261397943/history",
                "ancestors": "",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/261397943/descendant",
                "space": "/rest/api/space/SPACE"
            }
        }
    ],
    "container": {
        "id": 119963668,
        "key": "SPACE",
        "name": "Space",
        "type": "global",
        "_links": {
            "webui": "/display/SPACE",
            "self": "https://testcontext/wiki/cf/rest/api/space/SPACE"
        },
        "_expandable": {
            "metadata": "",
            "icon": "",
            "description": "",
            "homepage": "/rest/api/content/119239646"
        }
    },
    "body": {
        "storage": {
            "value": "<h2>This is a test</h2>",
            "representation": "storage",
            "_expandable": {
                "content": "/rest/api/content/388386403"
            }
        },
        "_expandable": {
            "editor": "",
            "view": "",
            "export_view": "",
            "styled_view": "",
            "anonymous_export_view": ""
        }
    },
    "extensions": {
        "position": "none"
    },
    "_links": {
        "webui": "/display/SPACE/A+new+page+title",
        "edit": "/pages/resumedraft.action?draftId=388386403",
        "tinyui": "/x/Y04mFw",
        "collection": "/rest/api/content",
        "base": "https://testcontext/wiki/cf",
        "context": "/wiki/cf",
        "self": "https://testcontext/wiki/cf/rest/api/content/388386403"
    },
    "_expandable": {
        "metadata": "",
        "operations": "",
        "children": "/rest/api/content/388386403/child",
        "restrictions": "/rest/api/content/388386403/restriction/byOperation",
        "descendants": "/rest/api/content/388386403/descendant"
    }
}
'''

UPDATE_PAGE_RESPONSE = '''
{
    "id": "1234",
    "type": "page",
    "status": "current",
    "title": "A new page title",
    "space": {
        "id": 119963668,
        "key": "SPACE",
        "name": "SPACE",
        "type": "global",
        "_links": {
            "webui": "/display/SPACE",
            "self": "https://testcontext/wiki/cf/rest/api/space/SPACE"
        },
        "_expandable": {
            "metadata": "",
            "icon": "",
            "description": "",
            "homepage": "/rest/api/content/119239646"
        }
    },
    "history": {
        "latest": true,
        "createdBy": {
            "type": "known",
            "username": "SPACE-auto",
            "userKey": "2c9239b948dc82440148dc875dc709a1",
            "profilePicture": {
                "path": "/wiki/cf/images/icons/profilepics/default.svg",
                "width": 48,
                "height": 48,
                "isDefault": true
            },
            "displayName": "SPACE-auto",
            "_links": {
                "self": "https://testcontext/wiki/cf/rest/api/user?key=2c9239b948dc82440148dc875dc709a1"
            },
            "_expandable": {
                "status": ""
            }
        },
        "createdDate": "2020-10-30T15:58:05.590Z",
        "_links": {
            "self": "https://testcontext/wiki/cf/rest/api/content/388386405/history"
        },
        "_expandable": {
            "lastUpdated": "",
            "previousVersion": "",
            "contributors": "",
            "nextVersion": ""
        }
    },
    "version": {
        "by": {
            "type": "known",
            "username": "SPACE-auto",
            "userKey": "2c9239b948dc82440148dc875dc709a1",
            "profilePicture": {
                "path": "/wiki/cf/images/icons/profilepics/default.svg",
                "width": 48,
                "height": 48,
                "isDefault": true
            },
            "displayName": "SPACE-auto",
            "_links": {
                "self": "https://testcontext/wiki/cf/rest/api/user?key=2c9239b948dc82440148dc875dc709a1"
            },
            "_expandable": {
                "status": ""
            }
        },
        "when": "2020-10-30T16:29:18.318Z",
        "number": 2,
        "minorEdit": false,
        "hidden": false,
        "_links": {
            "self": "https://testcontext/wiki/cf/rest/experimental/content/388386405/version/2"
        },
        "_expandable": {
            "content": "/rest/api/content/388386405"
        }
    },
    "ancestors": [
        {
            "id": "119239646",
            "type": "page",
            "status": "current",
            "title": "SPACE Home",
            "extensions": {
                "position": "none"
            },
            "_links": {
                "webui": "/display/SPACE/SPACE+Home",
                "edit": "/pages/resumedraft.action?draftId=119239646",
                "tinyui": "/x/3nMbBw",
                "self": "https://testcontext/wiki/cf/rest/api/content/119239646"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/119239646/child",
                "restrictions": "/rest/api/content/119239646/restriction/byOperation",
                "history": "/rest/api/content/119239646/history",
                "ancestors": "",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/119239646/descendant",
                "space": "/rest/api/space/SPACE"
            }
        },
        {
            "id": "148348821",
            "type": "page",
            "status": "current",
            "title": "Project SPACE",
            "extensions": {
                "position": 85
            },
            "_links": {
                "webui": "/display/SPACE/Project+SPACE",
                "edit": "/pages/resumedraft.action?draftId=148348821",
                "tinyui": "/x/lZ-XC",
                "self": "https://testcontext/wiki/cf/rest/api/content/148348821"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/148348821/child",
                "restrictions": "/rest/api/content/148348821/restriction/byOperation",
                "history": "/rest/api/content/148348821/history",
                "ancestors": "",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/148348821/descendant",
                "space": "/rest/api/space/SPACE"
            }
        },
        {
            "id": "148348824",
            "type": "page",
            "status": "current",
            "title": "Forthic",
            "extensions": {
                "position": "none"
            },
            "_links": {
                "webui": "/display/SPACE/Forthic",
                "edit": "/pages/resumedraft.action?draftId=148348824",
                "tinyui": "/x/mJ-XC",
                "self": "https://testcontext/wiki/cf/rest/api/content/148348824"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/148348824/child",
                "restrictions": "/rest/api/content/148348824/restriction/byOperation",
                "history": "/rest/api/content/148348824/history",
                "ancestors": "",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/148348824/descendant",
                "space": "/rest/api/space/SPACE"
            }
        },
        {
            "id": "248042769",
            "type": "page",
            "status": "current",
            "title": "Forthic Framework",
            "extensions": {
                "position": "none"
            },
            "_links": {
                "webui": "/display/SPACE/Forthic+Framework",
                "edit": "/pages/resumedraft.action?draftId=248042769&draftShareId=e6dfb6e9-9a98-4e20-8afc-8fdfaa354ace",
                "tinyui": "/x/EdXIDg",
                "self": "https://testcontext/wiki/cf/rest/api/content/248042769"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/248042769/child",
                "restrictions": "/rest/api/content/248042769/restriction/byOperation",
                "history": "/rest/api/content/248042769/history",
                "ancestors": "",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/248042769/descendant",
                "space": "/rest/api/space/SPACE"
            }
        },
        {
            "id": "261397943",
            "type": "page",
            "status": "current",
            "title": "Forthic Testing",
            "extensions": {
                "position": "none"
            },
            "_links": {
                "webui": "/display/SPACE/Forthic+Testing",
                "edit": "/pages/resumedraft.action?draftId=261397943&draftShareId=5ebe725d-0d69-46a5-bf3f-5cd01d7c17c7",
                "tinyui": "/x/t52UDw",
                "self": "https://testcontext/wiki/cf/rest/api/content/261397943"
            },
            "_expandable": {
                "container": "/rest/api/space/SPACE",
                "metadata": "",
                "operations": "",
                "children": "/rest/api/content/261397943/child",
                "restrictions": "/rest/api/content/261397943/restriction/byOperation",
                "history": "/rest/api/content/261397943/history",
                "ancestors": "",
                "body": "",
                "version": "",
                "descendants": "/rest/api/content/261397943/descendant",
                "space": "/rest/api/space/SPACE"
            }
        }
    ],
    "container": {
        "id": 119963668,
        "key": "SPACE",
        "name": "SPACE",
        "type": "global",
        "_links": {
            "webui": "/display/SPACE",
            "self": "https://testcontext/wiki/cf/rest/api/space/SPACE"
        },
        "_expandable": {
            "metadata": "",
            "icon": "",
            "description": "",
            "homepage": "/rest/api/content/119239646"
        }
    },
    "body": {
        "storage": {
            "value": "<h2>This is second a test</h2>",
            "representation": "storage",
            "_expandable": {
                "content": "/rest/api/content/388386405"
            }
        },
        "_expandable": {
            "editor": "",
            "view": "",
            "export_view": "",
            "styled_view": "",
            "anonymous_export_view": ""
        }
    },
    "extensions": {
        "position": "none"
    },
    "_links": {
        "webui": "/display/SPACE/A+new+page+title",
        "edit": "/pages/resumedraft.action?draftId=388386405",
        "tinyui": "/x/ZU4mFw",
        "collection": "/rest/api/content",
        "base": "https://testcontext/wiki/cf",
        "context": "/wiki/cf",
        "self": "https://testcontext/wiki/cf/rest/api/content/388386405"
    },
    "_expandable": {
        "metadata": "",
        "operations": "",
        "children": "/rest/api/content/388386405/child",
        "restrictions": "/rest/api/content/388386405/restriction/byOperation",
        "descendants": "/rest/api/content/388386405/descendant"
    }
}
'''