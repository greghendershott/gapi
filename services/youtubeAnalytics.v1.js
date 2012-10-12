{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "youtubeAnalytics:v1",
 "name": "youtubeAnalytics",
 "version": "v1",
 "revision": "20120816",
 "title": "YouTube Analytics API",
 "description": "Retrieve your YouTube Analytics reports.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/youtube-16.png",
  "x32": "http://www.google.com/images/icons/product/youtube-32.png"
 },
 "documentationLink": "http://developers.google.com/youtube/analytics/",
 "labels": [
  "graduated"
 ],
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/youtube/analytics/v1/",
 "basePath": "/youtube/analytics/v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "youtube/analytics/v1/",
 "batchPath": "batch",
 "parameters": {
  "alt": {
   "type": "string",
   "description": "Data format for the response.",
   "default": "json",
   "enum": [
    "csv",
    "json"
   ],
   "enumDescriptions": [
    "Responses with Content-Type of text/csv",
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
 "auth": {
  "oauth2": {
   "scopes": {
    "https://www.googleapis.com/auth/yt-analytics.readonly": {
     "description": "View YouTube Analytics reports for your YouTube content"
    }
   }
  }
 },
 "schemas": {
  "ResultTable": {
   "id": "ResultTable",
   "type": "object",
   "description": "Contains a single result table. The table is returned as an array of rows that contain the values for the cells of the table. Depending on the metric or dimension, the cell can contain a string (video ID, country code) or a number (number of views or number of likes).",
   "properties": {
    "columnHeaders": {
     "type": "array",
     "description": "Contains information about the columns returned in the \"rows\" fields. The order of the elements matches the order of the corresponding columns in \"rows\" field.",
     "items": {
      "type": "object",
      "properties": {
       "columnType": {
        "type": "string",
        "description": "The type of the column (DIMENSION, METRIC)."
       },
       "dataType": {
        "type": "string",
        "description": "Type of the data in the column (STRING, INTEGER, FLOAT)."
       },
       "name": {
        "type": "string",
        "description": "The name of the dimension or metric."
       }
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifier used to mark the structure as a result table.",
     "default": "youtubeAnalytics#resultTable"
    },
    "rows": {
     "type": "array",
     "description": "Contains all rows of the result table. Each row contains an array with the values for the columns. The order matches the order of the column information provided in the \"columnHeaders\" field. If no data is available for the given query, the \"rows\" element will be omitted from the response. The response for a query with the day dimension will not contain rows for the most recent days.",
     "items": {
      "type": "array",
      "items": {
       "type": "any"
      }
     }
    }
   }
  }
 },
 "resources": {
  "reports": {
   "methods": {
    "query": {
     "id": "youtubeAnalytics.reports.query",
     "path": "reports",
     "httpMethod": "GET",
     "description": "Retrieve your YouTube Analytics reports.",
     "parameters": {
      "dimensions": {
       "type": "string",
       "description": "A comma-separated list of YouTube Analytics dimensions. E.g., 'video', or 'ageGroup,gender'.",
       "pattern": "[0-9a-zA-Z,]+",
       "location": "query"
      },
      "end-date": {
       "type": "string",
       "description": "End date for fetching YouTube Analytics data. All requests should specify an end date formatted as YYYY-MM-DD.",
       "required": true,
       "pattern": "[0-9]{4}-[0-9]{2}-[0-9]{2}",
       "location": "query"
      },
      "filters": {
       "type": "string",
       "description": "A list of dimension filters to be applied to YouTube Analytics data. Multiple filters can be joined together with the ';' character. The returned result table will satisfy both filters. E.g., video==dMH0bHeiRNg;country==IT will restrict the returned stats to the given video and the country Italy.",
       "location": "query"
      },
      "ids": {
       "type": "string",
       "description": "Unique channel or content owner ID for retrieving YouTube Analytics data. Either channel==C or contentOwner==O where 'C' is the encrypted channel ID and 'O' is the content owner name.",
       "required": true,
       "pattern": "[a-zA-Z]+==[a-zA-Z0-9_+-]+",
       "location": "query"
      },
      "max-results": {
       "type": "integer",
       "description": "The maximum number of rows to include in the response.",
       "format": "int32",
       "minimum": "1",
       "location": "query"
      },
      "metrics": {
       "type": "string",
       "description": "A comma-separated list of YouTube Analytics metrics. E.g., 'views' or 'likes,dislikes'",
       "required": true,
       "pattern": "[0-9a-zA-Z,]+",
       "location": "query"
      },
      "sort": {
       "type": "string",
       "description": "A comma-separated list of dimensions or metrics that determine the sort order for YouTube Analytics data. By default the sort order is ascending, '-' prefix causes descending sort order.",
       "pattern": "(-)?[0-9a-zA-Z,]+",
       "location": "query"
      },
      "start-date": {
       "type": "string",
       "description": "Start date for fetching YouTube Analytics data. All requests should specify a start date formatted as YYYY-MM-DD.",
       "required": true,
       "pattern": "[0-9]{4}-[0-9]{2}-[0-9]{2}",
       "location": "query"
      },
      "start-index": {
       "type": "integer",
       "description": "An index of the first entity to retrieve. Use this parameter as a pagination mechanism along with the max-results parameter (one-based, inclusive).",
       "format": "int32",
       "minimum": "1",
       "location": "query"
      }
     },
     "parameterOrder": [
      "ids",
      "start-date",
      "end-date",
      "metrics"
     ],
     "response": {
      "$ref": "ResultTable"
     },
     "scopes": [
      "https://www.googleapis.com/auth/yt-analytics.readonly"
     ]
    }
   }
  }
 }
}
