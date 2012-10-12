{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "webfonts:v1",
 "name": "webfonts",
 "version": "v1",
 "revision": "20120614",
 "title": "Google Web Fonts Developer API",
 "description": "The Google Web Fonts Developer API.",
 "icons": {
  "x16": "http://www.google.com/images/icons/feature/font_api-16.png",
  "x32": "http://www.google.com/images/icons/feature/font_api-32.gif"
 },
 "documentationLink": "http://code.google.com/apis/webfonts/docs/developer_api.html",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/webfonts/v1/",
 "basePath": "/webfonts/v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "webfonts/v1/",
 "batchPath": "batch",
 "parameters": {
  "alt": {
   "type": "string",
   "description": "Data format for the response.",
   "default": "json",
   "enum": [
    "json"
   ],
   "enumDescriptions": [
    "Responses with Content-Type of application/json"
   ],
   "location": "query"
  },
  "fields": {
   "type": "string",
   "description": "Selector specifying which fields to include in a partial response.",
   "location": "query"
  },
  "key": {
   "type": "string",
   "description": "API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.",
   "location": "query"
  },
  "oauth_token": {
   "type": "string",
   "description": "OAuth 2.0 token for the current user.",
   "location": "query"
  },
  "prettyPrint": {
   "type": "boolean",
   "description": "Returns response with indentations and line breaks.",
   "default": "true",
   "location": "query"
  },
  "quotaUser": {
   "type": "string",
   "description": "Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters. Overrides userIp if both are provided.",
   "location": "query"
  },
  "userIp": {
   "type": "string",
   "description": "IP address of the site where the request originates. Use this if you want to enforce per-user limits.",
   "location": "query"
  }
 },
 "schemas": {
  "Webfont": {
   "id": "Webfont",
   "type": "object",
   "properties": {
    "family": {
     "type": "any",
     "description": "The name of the font."
    },
    "kind": {
     "type": "string",
     "description": "This kind represents a webfont object in the webfonts service.",
     "default": "webfonts#webfont"
    },
    "subsets": {
     "type": "any",
     "description": "The scripts supported by the font."
    },
    "variants": {
     "type": "any",
     "description": "The available variants for the font."
    }
   }
  },
  "WebfontList": {
   "id": "WebfontList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "The list of fonts currently served by the Google Fonts API.",
     "items": {
      "$ref": "Webfont"
     }
    },
    "kind": {
     "type": "string",
     "description": "This kind represents a list of webfont objects in the webfonts service.",
     "default": "webfonts#webfontList"
    }
   }
  }
 },
 "resources": {
  "webfonts": {
   "methods": {
    "list": {
     "id": "webfonts.webfonts.list",
     "path": "webfonts",
     "httpMethod": "GET",
     "description": "Retrieves the list of fonts currently served by the Google Web Fonts Developer API",
     "parameters": {
      "sort": {
       "type": "string",
       "description": "Enables sorting of the list",
       "enum": [
        "alpha",
        "date",
        "popularity",
        "style",
        "trending"
       ],
       "enumDescriptions": [
        "Sort alphabetically",
        "Sort by date added",
        "Sort by popularity",
        "Sort by number of styles",
        "Sort by trending"
       ],
       "location": "query"
      }
     },
     "response": {
      "$ref": "WebfontList"
     }
    }
   }
  }
 }
}
