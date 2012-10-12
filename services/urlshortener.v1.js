{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "urlshortener:v1",
 "name": "urlshortener",
 "version": "v1",
 "title": "URL Shortener API",
 "description": "Lets you create, inspect, and manage goo.gl short URLs",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/search-16.gif",
  "x32": "http://www.google.com/images/icons/product/search-32.gif"
 },
 "documentationLink": "http://code.google.com/apis/urlshortener/v1/getting_started.html",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/urlshortener/v1/",
 "basePath": "/urlshortener/v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "urlshortener/v1/",
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
 "auth": {
  "oauth2": {
   "scopes": {
    "https://www.googleapis.com/auth/urlshortener": {
     "description": "Manage your goo.gl short URLs"
    }
   }
  }
 },
 "schemas": {
  "AnalyticsSnapshot": {
   "id": "AnalyticsSnapshot",
   "type": "object",
   "properties": {
    "browsers": {
     "type": "array",
     "description": "Top browsers, e.g. \"Chrome\"; sorted by (descending) click counts. Only present if this data is available.",
     "items": {
      "$ref": "StringCount"
     }
    },
    "countries": {
     "type": "array",
     "description": "Top countries (expressed as country codes), e.g. \"US\" or \"DE\"; sorted by (descending) click counts. Only present if this data is available.",
     "items": {
      "$ref": "StringCount"
     }
    },
    "longUrlClicks": {
     "type": "string",
     "description": "Number of clicks on all goo.gl short URLs pointing to this long URL.",
     "format": "int64"
    },
    "platforms": {
     "type": "array",
     "description": "Top platforms or OSes, e.g. \"Windows\"; sorted by (descending) click counts. Only present if this data is available.",
     "items": {
      "$ref": "StringCount"
     }
    },
    "referrers": {
     "type": "array",
     "description": "Top referring hosts, e.g. \"www.google.com\"; sorted by (descending) click counts. Only present if this data is available.",
     "items": {
      "$ref": "StringCount"
     }
    },
    "shortUrlClicks": {
     "type": "string",
     "description": "Number of clicks on this short URL.",
     "format": "int64"
    }
   }
  },
  "AnalyticsSummary": {
   "id": "AnalyticsSummary",
   "type": "object",
   "properties": {
    "allTime": {
     "$ref": "AnalyticsSnapshot",
     "description": "Click analytics over all time."
    },
    "day": {
     "$ref": "AnalyticsSnapshot",
     "description": "Click analytics over the last day."
    },
    "month": {
     "$ref": "AnalyticsSnapshot",
     "description": "Click analytics over the last month."
    },
    "twoHours": {
     "$ref": "AnalyticsSnapshot",
     "description": "Click analytics over the last two hours."
    },
    "week": {
     "$ref": "AnalyticsSnapshot",
     "description": "Click analytics over the last week."
    }
   }
  },
  "StringCount": {
   "id": "StringCount",
   "type": "object",
   "properties": {
    "count": {
     "type": "string",
     "description": "Number of clicks for this top entry, e.g. for this particular country or browser.",
     "format": "int64"
    },
    "id": {
     "type": "string",
     "description": "Label assigned to this top entry, e.g. \"US\" or \"Chrome\"."
    }
   }
  },
  "Url": {
   "id": "Url",
   "type": "object",
   "properties": {
    "analytics": {
     "$ref": "AnalyticsSummary",
     "description": "A summary of the click analytics for the short and long URL. Might not be present if not requested or currently unavailable."
    },
    "created": {
     "type": "string",
     "description": "Time the short URL was created; ISO 8601 representation using the yyyy-MM-dd'T'HH:mm:ss.SSSZZ format, e.g. \"2010-10-14T19:01:24.944+00:00\"."
    },
    "id": {
     "type": "string",
     "description": "Short URL, e.g. \"http://goo.gl/l6MS\"."
    },
    "kind": {
     "type": "string",
     "description": "The fixed string \"urlshortener#url\".",
     "default": "urlshortener#url"
    },
    "longUrl": {
     "type": "string",
     "description": "Long URL, e.g. \"http://www.google.com/\". Might not be present if the status is \"REMOVED\"."
    },
    "status": {
     "type": "string",
     "description": "Status of the target URL. Possible values: \"OK\", \"MALWARE\", \"PHISHING\", or \"REMOVED\". A URL might be marked \"REMOVED\" if it was flagged as spam, for example."
    }
   }
  },
  "UrlHistory": {
   "id": "UrlHistory",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of URL resources.",
     "items": {
      "$ref": "Url"
     }
    },
    "itemsPerPage": {
     "type": "integer",
     "description": "Number of items returned with each full \"page\" of results. Note that the last page could have fewer items than the \"itemsPerPage\" value.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "The fixed string \"urlshortener#urlHistory\".",
     "default": "urlshortener#urlHistory"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token to provide to get the next page of results."
    },
    "totalItems": {
     "type": "integer",
     "description": "Total number of short URLs associated with this user (may be approximate).",
     "format": "int32"
    }
   }
  }
 },
 "resources": {
  "url": {
   "methods": {
    "get": {
     "id": "urlshortener.url.get",
     "path": "url",
     "httpMethod": "GET",
     "description": "Expands a short URL or gets creation time and analytics.",
     "parameters": {
      "projection": {
       "type": "string",
       "description": "Additional information to return.",
       "enum": [
        "ANALYTICS_CLICKS",
        "ANALYTICS_TOP_STRINGS",
        "FULL"
       ],
       "enumDescriptions": [
        "Returns only click counts.",
        "Returns only top string counts.",
        "Returns the creation timestamp and all available analytics."
       ],
       "location": "query"
      },
      "shortUrl": {
       "type": "string",
       "description": "The short URL, including the protocol.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "shortUrl"
     ],
     "response": {
      "$ref": "Url"
     }
    },
    "insert": {
     "id": "urlshortener.url.insert",
     "path": "url",
     "httpMethod": "POST",
     "description": "Creates a new short URL.",
     "request": {
      "$ref": "Url"
     },
     "response": {
      "$ref": "Url"
     },
     "scopes": [
      "https://www.googleapis.com/auth/urlshortener"
     ]
    },
    "list": {
     "id": "urlshortener.url.list",
     "path": "url/history",
     "httpMethod": "GET",
     "description": "Retrieves a list of URLs shortened by a user.",
     "parameters": {
      "projection": {
       "type": "string",
       "description": "Additional information to return.",
       "enum": [
        "ANALYTICS_CLICKS",
        "FULL"
       ],
       "enumDescriptions": [
        "Returns short URL click counts.",
        "Returns short URL click counts."
       ],
       "location": "query"
      },
      "start-token": {
       "type": "string",
       "description": "Token for requesting successive pages of results.",
       "location": "query"
      }
     },
     "response": {
      "$ref": "UrlHistory"
     },
     "scopes": [
      "https://www.googleapis.com/auth/urlshortener"
     ]
    }
   }
  }
 }
}
