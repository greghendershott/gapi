{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "adsense:v1.1",
 "name": "adsense",
 "canonicalName": "AdSense",
 "version": "v1.1",
 "revision": "20121007",
 "title": "AdSense Management API",
 "description": "Gives AdSense publishers access to their inventory and the ability to generate reports",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/adsense-16.png",
  "x32": "http://www.google.com/images/icons/product/adsense-32.png"
 },
 "documentationLink": "https://developers.google.com/adsense/management/",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/adsense/v1.1/",
 "basePath": "/adsense/v1.1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "adsense/v1.1/",
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
    "https://www.googleapis.com/auth/adsense": {
     "description": "View and manage your AdSense data"
    },
    "https://www.googleapis.com/auth/adsense.readonly": {
     "description": "View your AdSense data"
    }
   }
  }
 },
 "schemas": {
  "Account": {
   "id": "Account",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier of this account."
    },
    "kind": {
     "type": "string",
     "description": "Kind of resource this is, in this case adsense#account.",
     "default": "adsense#account"
    },
    "name": {
     "type": "string",
     "description": "Name of this account."
    },
    "subAccounts": {
     "type": "array",
     "description": "Sub accounts of the this account.",
     "items": {
      "$ref": "Account"
     }
    }
   }
  },
  "Accounts": {
   "id": "Accounts",
   "type": "object",
   "properties": {
    "etag": {
     "type": "string",
     "description": "ETag of this response for caching purposes."
    },
    "items": {
     "type": "array",
     "description": "The accounts returned in this list response.",
     "items": {
      "$ref": "Account"
     }
    },
    "kind": {
     "type": "string",
     "description": "Kind of list this is, in this case adsense#accounts.",
     "default": "adsense#accounts"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through accounts. To retrieve the next page of results, set the next request's \"pageToken\" value to this."
    }
   }
  },
  "AdClient": {
   "id": "AdClient",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier of this ad client."
    },
    "kind": {
     "type": "string",
     "description": "Kind of resource this is, in this case adsense#adClient.",
     "default": "adsense#adClient"
    },
    "productCode": {
     "type": "string",
     "description": "This ad client's product code, which corresponds to the PRODUCT_CODE report dimension."
    },
    "supportsReporting": {
     "type": "boolean",
     "description": "Whether this ad client supports being reported on."
    }
   }
  },
  "AdClients": {
   "id": "AdClients",
   "type": "object",
   "properties": {
    "etag": {
     "type": "string",
     "description": "ETag of this response for caching purposes."
    },
    "items": {
     "type": "array",
     "description": "The ad clients returned in this list response.",
     "items": {
      "$ref": "AdClient"
     }
    },
    "kind": {
     "type": "string",
     "description": "Kind of list this is, in this case adsense#adClients.",
     "default": "adsense#adClients"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through ad clients. To retrieve the next page of results, set the next request's \"pageToken\" value to this."
    }
   }
  },
  "AdUnit": {
   "id": "AdUnit",
   "type": "object",
   "properties": {
    "code": {
     "type": "string",
     "description": "Identity code of this ad unit, not necessarily unique across ad clients."
    },
    "id": {
     "type": "string",
     "description": "Unique identifier of this ad unit. This should be considered an opaque identifier; it is not safe to rely on it being in any particular format."
    },
    "kind": {
     "type": "string",
     "description": "Kind of resource this is, in this case adsense#adUnit.",
     "default": "adsense#adUnit"
    },
    "name": {
     "type": "string",
     "description": "Name of this ad unit."
    },
    "status": {
     "type": "string",
     "description": "Status of this ad unit. Possible values are:\nNEW: Indicates that the ad unit was created within the last seven days and does not yet have any activity associated with it.\n\nACTIVE: Indicates that there has been activity on this ad unit in the last seven days.\n\nINACTIVE: Indicates that there has been no activity on this ad unit in the last seven days."
    }
   }
  },
  "AdUnits": {
   "id": "AdUnits",
   "type": "object",
   "properties": {
    "etag": {
     "type": "string",
     "description": "ETag of this response for caching purposes."
    },
    "items": {
     "type": "array",
     "description": "The ad units returned in this list response.",
     "items": {
      "$ref": "AdUnit"
     }
    },
    "kind": {
     "type": "string",
     "description": "Kind of list this is, in this case adsense#adUnits.",
     "default": "adsense#adUnits"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through ad units. To retrieve the next page of results, set the next request's \"pageToken\" value to this."
    }
   }
  },
  "AdsenseReportsGenerateResponse": {
   "id": "AdsenseReportsGenerateResponse",
   "type": "object",
   "properties": {
    "averages": {
     "type": "array",
     "description": "The averages of the report. This is the same length as any other row in the report; cells corresponding to dimension columns are empty.",
     "items": {
      "type": "string"
     }
    },
    "headers": {
     "type": "array",
     "description": "The header information of the columns requested in the report. This is a list of headers; one for each dimension in the request, followed by one for each metric in the request.",
     "items": {
      "type": "object",
      "properties": {
       "currency": {
        "type": "string",
        "description": "The currency of this column. Only present if the header type is METRIC_CURRENCY."
       },
       "name": {
        "type": "string",
        "description": "The name of the header."
       },
       "type": {
        "type": "string",
        "description": "The type of the header; one of DIMENSION, METRIC_TALLY, METRIC_RATIO, or METRIC_CURRENCY."
       }
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "Kind this is, in this case adsense#report.",
     "default": "adsense#report"
    },
    "rows": {
     "type": "array",
     "description": "The output rows of the report. Each row is a list of cells; one for each dimension in the request, followed by one for each metric in the request. The dimension cells contain strings, and the metric cells contain numbers.",
     "items": {
      "type": "array",
      "items": {
       "type": "string"
      }
     }
    },
    "totalMatchedRows": {
     "type": "string",
     "description": "The total number of rows matched by the report request. Fewer rows may be returned in the response due to being limited by the row count requested or the report row limit.",
     "format": "int64"
    },
    "totals": {
     "type": "array",
     "description": "The totals of the report. This is the same length as any other row in the report; cells corresponding to dimension columns are empty.",
     "items": {
      "type": "string"
     }
    },
    "warnings": {
     "type": "array",
     "description": "Any warnings associated with generation of the report.",
     "items": {
      "type": "string"
     }
    }
   }
  },
  "CustomChannel": {
   "id": "CustomChannel",
   "type": "object",
   "properties": {
    "code": {
     "type": "string",
     "description": "Code of this custom channel, not necessarily unique across ad clients."
    },
    "id": {
     "type": "string",
     "description": "Unique identifier of this custom channel. This should be considered an opaque identifier; it is not safe to rely on it being in any particular format."
    },
    "kind": {
     "type": "string",
     "description": "Kind of resource this is, in this case adsense#customChannel.",
     "default": "adsense#customChannel"
    },
    "name": {
     "type": "string",
     "description": "Name of this custom channel."
    },
    "targetingInfo": {
     "type": "object",
     "description": "The targeting information of this custom channel, if activated.",
     "properties": {
      "adsAppearOn": {
       "type": "string",
       "description": "The name used to describe this channel externally."
      },
      "description": {
       "type": "string",
       "description": "The external description of the channel."
      },
      "location": {
       "type": "string",
       "description": "The locations in which ads appear. (Only valid for content and mobile content ads). Acceptable values for content ads are: TOP_LEFT, TOP_CENTER, TOP_RIGHT, MIDDLE_LEFT, MIDDLE_CENTER, MIDDLE_RIGHT, BOTTOM_LEFT, BOTTOM_CENTER, BOTTOM_RIGHT, MULTIPLE_LOCATIONS. Acceptable values for mobile content ads are: TOP, MIDDLE, BOTTOM, MULTIPLE_LOCATIONS."
      },
      "siteLanguage": {
       "type": "string",
       "description": "The language of the sites ads will be displayed on."
      }
     }
    }
   }
  },
  "CustomChannels": {
   "id": "CustomChannels",
   "type": "object",
   "properties": {
    "etag": {
     "type": "string",
     "description": "ETag of this response for caching purposes."
    },
    "items": {
     "type": "array",
     "description": "The custom channels returned in this list response.",
     "items": {
      "$ref": "CustomChannel"
     }
    },
    "kind": {
     "type": "string",
     "description": "Kind of list this is, in this case adsense#customChannels.",
     "default": "adsense#customChannels"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through custom channels. To retrieve the next page of results, set the next request's \"pageToken\" value to this."
    }
   }
  },
  "UrlChannel": {
   "id": "UrlChannel",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier of this URL channel. This should be considered an opaque identifier; it is not safe to rely on it being in any particular format."
    },
    "kind": {
     "type": "string",
     "description": "Kind of resource this is, in this case adsense#urlChannel.",
     "default": "adsense#urlChannel"
    },
    "urlPattern": {
     "type": "string",
     "description": "URL Pattern of this URL channel. Does not include \"http://\" or \"https://\". Example: www.example.com/home"
    }
   }
  },
  "UrlChannels": {
   "id": "UrlChannels",
   "type": "object",
   "properties": {
    "etag": {
     "type": "string",
     "description": "ETag of this response for caching purposes."
    },
    "items": {
     "type": "array",
     "description": "The URL channels returned in this list response.",
     "items": {
      "$ref": "UrlChannel"
     }
    },
    "kind": {
     "type": "string",
     "description": "Kind of list this is, in this case adsense#urlChannels.",
     "default": "adsense#urlChannels"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through URL channels. To retrieve the next page of results, set the next request's \"pageToken\" value to this."
    }
   }
  }
 },
 "resources": {
  "accounts": {
   "methods": {
    "get": {
     "id": "adsense.accounts.get",
     "path": "accounts/{accountId}",
     "httpMethod": "GET",
     "description": "Get information about the selected AdSense account.",
     "parameters": {
      "accountId": {
       "type": "string",
       "description": "Account to get information about.",
       "required": true,
       "location": "path"
      },
      "tree": {
       "type": "boolean",
       "description": "Whether the tree of sub accounts should be returned.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "accountId"
     ],
     "response": {
      "$ref": "Account"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsense",
      "https://www.googleapis.com/auth/adsense.readonly"
     ]
    },
    "list": {
     "id": "adsense.accounts.list",
     "path": "accounts",
     "httpMethod": "GET",
     "description": "List all accounts available to this AdSense account.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of accounts to include in the response, used for paging.",
       "format": "int32",
       "minimum": "0",
       "maximum": "10000",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token, used to page through accounts. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
       "location": "query"
      }
     },
     "response": {
      "$ref": "Accounts"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsense",
      "https://www.googleapis.com/auth/adsense.readonly"
     ]
    }
   },
   "resources": {
    "adclients": {
     "methods": {
      "list": {
       "id": "adsense.accounts.adclients.list",
       "path": "accounts/{accountId}/adclients",
       "httpMethod": "GET",
       "description": "List all ad clients in the specified account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account for which to list ad clients.",
         "required": true,
         "location": "path"
        },
        "maxResults": {
         "type": "integer",
         "description": "The maximum number of ad clients to include in the response, used for paging.",
         "format": "int32",
         "minimum": "0",
         "maximum": "10000",
         "location": "query"
        },
        "pageToken": {
         "type": "string",
         "description": "A continuation token, used to page through ad clients. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "accountId"
       ],
       "response": {
        "$ref": "AdClients"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsense",
        "https://www.googleapis.com/auth/adsense.readonly"
       ]
      }
     }
    },
    "adunits": {
     "methods": {
      "get": {
       "id": "adsense.accounts.adunits.get",
       "path": "accounts/{accountId}/adclients/{adClientId}/adunits/{adUnitId}",
       "httpMethod": "GET",
       "description": "Gets the specified ad unit in the specified ad client for the specified account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account to which the ad client belongs.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client for which to get the ad unit.",
         "required": true,
         "location": "path"
        },
        "adUnitId": {
         "type": "string",
         "description": "Ad unit to retrieve.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "accountId",
        "adClientId",
        "adUnitId"
       ],
       "response": {
        "$ref": "AdUnit"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsense",
        "https://www.googleapis.com/auth/adsense.readonly"
       ]
      },
      "list": {
       "id": "adsense.accounts.adunits.list",
       "path": "accounts/{accountId}/adclients/{adClientId}/adunits",
       "httpMethod": "GET",
       "description": "List all ad units in the specified ad client for the specified account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account to which the ad client belongs.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client for which to list ad units.",
         "required": true,
         "location": "path"
        },
        "includeInactive": {
         "type": "boolean",
         "description": "Whether to include inactive ad units. Default: true.",
         "location": "query"
        },
        "maxResults": {
         "type": "integer",
         "description": "The maximum number of ad units to include in the response, used for paging.",
         "format": "int32",
         "minimum": "0",
         "maximum": "10000",
         "location": "query"
        },
        "pageToken": {
         "type": "string",
         "description": "A continuation token, used to page through ad units. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "accountId",
        "adClientId"
       ],
       "response": {
        "$ref": "AdUnits"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsense",
        "https://www.googleapis.com/auth/adsense.readonly"
       ]
      }
     },
     "resources": {
      "customchannels": {
       "methods": {
        "list": {
         "id": "adsense.accounts.adunits.customchannels.list",
         "path": "accounts/{accountId}/adclients/{adClientId}/adunits/{adUnitId}/customchannels",
         "httpMethod": "GET",
         "description": "List all custom channels which the specified ad unit belongs to.",
         "parameters": {
          "accountId": {
           "type": "string",
           "description": "Account to which the ad client belongs.",
           "required": true,
           "location": "path"
          },
          "adClientId": {
           "type": "string",
           "description": "Ad client which contains the ad unit.",
           "required": true,
           "location": "path"
          },
          "adUnitId": {
           "type": "string",
           "description": "Ad unit for which to list custom channels.",
           "required": true,
           "location": "path"
          },
          "maxResults": {
           "type": "integer",
           "description": "The maximum number of custom channels to include in the response, used for paging.",
           "format": "int32",
           "minimum": "0",
           "maximum": "10000",
           "location": "query"
          },
          "pageToken": {
           "type": "string",
           "description": "A continuation token, used to page through custom channels. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
           "location": "query"
          }
         },
         "parameterOrder": [
          "accountId",
          "adClientId",
          "adUnitId"
         ],
         "response": {
          "$ref": "CustomChannels"
         },
         "scopes": [
          "https://www.googleapis.com/auth/adsense",
          "https://www.googleapis.com/auth/adsense.readonly"
         ]
        }
       }
      }
     }
    },
    "customchannels": {
     "methods": {
      "get": {
       "id": "adsense.accounts.customchannels.get",
       "path": "accounts/{accountId}/adclients/{adClientId}/customchannels/{customChannelId}",
       "httpMethod": "GET",
       "description": "Get the specified custom channel from the specified ad client for the specified account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account to which the ad client belongs.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client which contains the custom channel.",
         "required": true,
         "location": "path"
        },
        "customChannelId": {
         "type": "string",
         "description": "Custom channel to retrieve.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "accountId",
        "adClientId",
        "customChannelId"
       ],
       "response": {
        "$ref": "CustomChannel"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsense",
        "https://www.googleapis.com/auth/adsense.readonly"
       ]
      },
      "list": {
       "id": "adsense.accounts.customchannels.list",
       "path": "accounts/{accountId}/adclients/{adClientId}/customchannels",
       "httpMethod": "GET",
       "description": "List all custom channels in the specified ad client for the specified account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account to which the ad client belongs.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client for which to list custom channels.",
         "required": true,
         "location": "path"
        },
        "maxResults": {
         "type": "integer",
         "description": "The maximum number of custom channels to include in the response, used for paging.",
         "format": "int32",
         "minimum": "0",
         "maximum": "10000",
         "location": "query"
        },
        "pageToken": {
         "type": "string",
         "description": "A continuation token, used to page through custom channels. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "accountId",
        "adClientId"
       ],
       "response": {
        "$ref": "CustomChannels"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsense",
        "https://www.googleapis.com/auth/adsense.readonly"
       ]
      }
     },
     "resources": {
      "adunits": {
       "methods": {
        "list": {
         "id": "adsense.accounts.customchannels.adunits.list",
         "path": "accounts/{accountId}/adclients/{adClientId}/customchannels/{customChannelId}/adunits",
         "httpMethod": "GET",
         "description": "List all ad units in the specified custom channel.",
         "parameters": {
          "accountId": {
           "type": "string",
           "description": "Account to which the ad client belongs.",
           "required": true,
           "location": "path"
          },
          "adClientId": {
           "type": "string",
           "description": "Ad client which contains the custom channel.",
           "required": true,
           "location": "path"
          },
          "customChannelId": {
           "type": "string",
           "description": "Custom channel for which to list ad units.",
           "required": true,
           "location": "path"
          },
          "includeInactive": {
           "type": "boolean",
           "description": "Whether to include inactive ad units. Default: true.",
           "location": "query"
          },
          "maxResults": {
           "type": "integer",
           "description": "The maximum number of ad units to include in the response, used for paging.",
           "format": "int32",
           "minimum": "0",
           "maximum": "10000",
           "location": "query"
          },
          "pageToken": {
           "type": "string",
           "description": "A continuation token, used to page through ad units. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
           "location": "query"
          }
         },
         "parameterOrder": [
          "accountId",
          "adClientId",
          "customChannelId"
         ],
         "response": {
          "$ref": "AdUnits"
         },
         "scopes": [
          "https://www.googleapis.com/auth/adsense",
          "https://www.googleapis.com/auth/adsense.readonly"
         ]
        }
       }
      }
     }
    },
    "reports": {
     "methods": {
      "generate": {
       "id": "adsense.accounts.reports.generate",
       "path": "accounts/{accountId}/reports",
       "httpMethod": "GET",
       "description": "Generate an AdSense report based on the report request sent in the query parameters. Returns the result as JSON; to retrieve output in CSV format specify \"alt=csv\" as a query parameter.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account upon which to report.",
         "required": true,
         "location": "path"
        },
        "currency": {
         "type": "string",
         "description": "Optional currency to use when reporting on monetary metrics. Defaults to the account's currency if not set.",
         "pattern": "[a-zA-Z]+",
         "location": "query"
        },
        "dimension": {
         "type": "string",
         "description": "Dimensions to base the report on.",
         "pattern": "[a-zA-Z_]+",
         "repeated": true,
         "location": "query"
        },
        "endDate": {
         "type": "string",
         "description": "End of the date range to report on in \"YYYY-MM-DD\" format, inclusive.",
         "required": true,
         "pattern": "\\d{4}-\\d{2}-\\d{2}|(today|startOfMonth|startOfYear)(([\\-\\+]\\d+[dwmy]){0,2}?)",
         "location": "query"
        },
        "filter": {
         "type": "string",
         "description": "Filters to be run on the report.",
         "pattern": "[a-zA-Z_]+(==|=@).+",
         "repeated": true,
         "location": "query"
        },
        "locale": {
         "type": "string",
         "description": "Optional locale to use for translating report output to a local language. Defaults to \"en_US\" if not specified.",
         "pattern": "[a-zA-Z_]+",
         "location": "query"
        },
        "maxResults": {
         "type": "integer",
         "description": "The maximum number of rows of report data to return.",
         "format": "int32",
         "minimum": "0",
         "maximum": "50000",
         "location": "query"
        },
        "metric": {
         "type": "string",
         "description": "Numeric columns to include in the report.",
         "pattern": "[a-zA-Z_]+",
         "repeated": true,
         "location": "query"
        },
        "sort": {
         "type": "string",
         "description": "The name of a dimension or metric to sort the resulting report on, optionally prefixed with \"+\" to sort ascending or \"-\" to sort descending. If no prefix is specified, the column is sorted ascending.",
         "pattern": "(\\+|-)?[a-zA-Z_]+",
         "repeated": true,
         "location": "query"
        },
        "startDate": {
         "type": "string",
         "description": "Start of the date range to report on in \"YYYY-MM-DD\" format, inclusive.",
         "required": true,
         "pattern": "\\d{4}-\\d{2}-\\d{2}|(today|startOfMonth|startOfYear)(([\\-\\+]\\d+[dwmy]){0,2}?)",
         "location": "query"
        },
        "startIndex": {
         "type": "integer",
         "description": "Index of the first row of report data to return.",
         "format": "int32",
         "minimum": "0",
         "maximum": "5000",
         "location": "query"
        }
       },
       "parameterOrder": [
        "accountId",
        "startDate",
        "endDate"
       ],
       "response": {
        "$ref": "AdsenseReportsGenerateResponse"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsense",
        "https://www.googleapis.com/auth/adsense.readonly"
       ],
       "supportsMediaDownload": true
      }
     }
    },
    "urlchannels": {
     "methods": {
      "list": {
       "id": "adsense.accounts.urlchannels.list",
       "path": "accounts/{accountId}/adclients/{adClientId}/urlchannels",
       "httpMethod": "GET",
       "description": "List all URL channels in the specified ad client for the specified account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account to which the ad client belongs.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client for which to list URL channels.",
         "required": true,
         "location": "path"
        },
        "maxResults": {
         "type": "integer",
         "description": "The maximum number of URL channels to include in the response, used for paging.",
         "format": "int32",
         "minimum": "0",
         "maximum": "10000",
         "location": "query"
        },
        "pageToken": {
         "type": "string",
         "description": "A continuation token, used to page through URL channels. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "accountId",
        "adClientId"
       ],
       "response": {
        "$ref": "UrlChannels"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsense",
        "https://www.googleapis.com/auth/adsense.readonly"
       ]
      }
     }
    }
   }
  },
  "adclients": {
   "methods": {
    "list": {
     "id": "adsense.adclients.list",
     "path": "adclients",
     "httpMethod": "GET",
     "description": "List all ad clients in this AdSense account.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of ad clients to include in the response, used for paging.",
       "format": "int32",
       "minimum": "0",
       "maximum": "10000",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token, used to page through ad clients. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
       "location": "query"
      }
     },
     "response": {
      "$ref": "AdClients"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsense",
      "https://www.googleapis.com/auth/adsense.readonly"
     ]
    }
   }
  },
  "adunits": {
   "methods": {
    "get": {
     "id": "adsense.adunits.get",
     "path": "adclients/{adClientId}/adunits/{adUnitId}",
     "httpMethod": "GET",
     "description": "Gets the specified ad unit in the specified ad client.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client for which to get the ad unit.",
       "required": true,
       "location": "path"
      },
      "adUnitId": {
       "type": "string",
       "description": "Ad unit to retrieve.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "adClientId",
      "adUnitId"
     ],
     "response": {
      "$ref": "AdUnit"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsense",
      "https://www.googleapis.com/auth/adsense.readonly"
     ]
    },
    "list": {
     "id": "adsense.adunits.list",
     "path": "adclients/{adClientId}/adunits",
     "httpMethod": "GET",
     "description": "List all ad units in the specified ad client for this AdSense account.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client for which to list ad units.",
       "required": true,
       "location": "path"
      },
      "includeInactive": {
       "type": "boolean",
       "description": "Whether to include inactive ad units. Default: true.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of ad units to include in the response, used for paging.",
       "format": "int32",
       "minimum": "0",
       "maximum": "10000",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token, used to page through ad units. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "adClientId"
     ],
     "response": {
      "$ref": "AdUnits"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsense",
      "https://www.googleapis.com/auth/adsense.readonly"
     ]
    }
   },
   "resources": {
    "customchannels": {
     "methods": {
      "list": {
       "id": "adsense.adunits.customchannels.list",
       "path": "adclients/{adClientId}/adunits/{adUnitId}/customchannels",
       "httpMethod": "GET",
       "description": "List all custom channels which the specified ad unit belongs to.",
       "parameters": {
        "adClientId": {
         "type": "string",
         "description": "Ad client which contains the ad unit.",
         "required": true,
         "location": "path"
        },
        "adUnitId": {
         "type": "string",
         "description": "Ad unit for which to list custom channels.",
         "required": true,
         "location": "path"
        },
        "maxResults": {
         "type": "integer",
         "description": "The maximum number of custom channels to include in the response, used for paging.",
         "format": "int32",
         "minimum": "0",
         "maximum": "10000",
         "location": "query"
        },
        "pageToken": {
         "type": "string",
         "description": "A continuation token, used to page through custom channels. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "adClientId",
        "adUnitId"
       ],
       "response": {
        "$ref": "CustomChannels"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsense",
        "https://www.googleapis.com/auth/adsense.readonly"
       ]
      }
     }
    }
   }
  },
  "customchannels": {
   "methods": {
    "get": {
     "id": "adsense.customchannels.get",
     "path": "adclients/{adClientId}/customchannels/{customChannelId}",
     "httpMethod": "GET",
     "description": "Get the specified custom channel from the specified ad client.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client which contains the custom channel.",
       "required": true,
       "location": "path"
      },
      "customChannelId": {
       "type": "string",
       "description": "Custom channel to retrieve.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "adClientId",
      "customChannelId"
     ],
     "response": {
      "$ref": "CustomChannel"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsense",
      "https://www.googleapis.com/auth/adsense.readonly"
     ]
    },
    "list": {
     "id": "adsense.customchannels.list",
     "path": "adclients/{adClientId}/customchannels",
     "httpMethod": "GET",
     "description": "List all custom channels in the specified ad client for this AdSense account.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client for which to list custom channels.",
       "required": true,
       "location": "path"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of custom channels to include in the response, used for paging.",
       "format": "int32",
       "minimum": "0",
       "maximum": "10000",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token, used to page through custom channels. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "adClientId"
     ],
     "response": {
      "$ref": "CustomChannels"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsense",
      "https://www.googleapis.com/auth/adsense.readonly"
     ]
    }
   },
   "resources": {
    "adunits": {
     "methods": {
      "list": {
       "id": "adsense.customchannels.adunits.list",
       "path": "adclients/{adClientId}/customchannels/{customChannelId}/adunits",
       "httpMethod": "GET",
       "description": "List all ad units in the specified custom channel.",
       "parameters": {
        "adClientId": {
         "type": "string",
         "description": "Ad client which contains the custom channel.",
         "required": true,
         "location": "path"
        },
        "customChannelId": {
         "type": "string",
         "description": "Custom channel for which to list ad units.",
         "required": true,
         "location": "path"
        },
        "includeInactive": {
         "type": "boolean",
         "description": "Whether to include inactive ad units. Default: true.",
         "location": "query"
        },
        "maxResults": {
         "type": "integer",
         "description": "The maximum number of ad units to include in the response, used for paging.",
         "format": "int32",
         "minimum": "0",
         "maximum": "10000",
         "location": "query"
        },
        "pageToken": {
         "type": "string",
         "description": "A continuation token, used to page through ad units. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "adClientId",
        "customChannelId"
       ],
       "response": {
        "$ref": "AdUnits"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsense",
        "https://www.googleapis.com/auth/adsense.readonly"
       ]
      }
     }
    }
   }
  },
  "reports": {
   "methods": {
    "generate": {
     "id": "adsense.reports.generate",
     "path": "reports",
     "httpMethod": "GET",
     "description": "Generate an AdSense report based on the report request sent in the query parameters. Returns the result as JSON; to retrieve output in CSV format specify \"alt=csv\" as a query parameter.",
     "parameters": {
      "accountId": {
       "type": "string",
       "description": "Accounts upon which to report.",
       "repeated": true,
       "location": "query"
      },
      "currency": {
       "type": "string",
       "description": "Optional currency to use when reporting on monetary metrics. Defaults to the account's currency if not set.",
       "pattern": "[a-zA-Z]+",
       "location": "query"
      },
      "dimension": {
       "type": "string",
       "description": "Dimensions to base the report on.",
       "pattern": "[a-zA-Z_]+",
       "repeated": true,
       "location": "query"
      },
      "endDate": {
       "type": "string",
       "description": "End of the date range to report on in \"YYYY-MM-DD\" format, inclusive.",
       "required": true,
       "pattern": "\\d{4}-\\d{2}-\\d{2}|(today|startOfMonth|startOfYear)(([\\-\\+]\\d+[dwmy]){0,2}?)",
       "location": "query"
      },
      "filter": {
       "type": "string",
       "description": "Filters to be run on the report.",
       "pattern": "[a-zA-Z_]+(==|=@).+",
       "repeated": true,
       "location": "query"
      },
      "locale": {
       "type": "string",
       "description": "Optional locale to use for translating report output to a local language. Defaults to \"en_US\" if not specified.",
       "pattern": "[a-zA-Z_]+",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of rows of report data to return.",
       "format": "int32",
       "minimum": "0",
       "maximum": "50000",
       "location": "query"
      },
      "metric": {
       "type": "string",
       "description": "Numeric columns to include in the report.",
       "pattern": "[a-zA-Z_]+",
       "repeated": true,
       "location": "query"
      },
      "sort": {
       "type": "string",
       "description": "The name of a dimension or metric to sort the resulting report on, optionally prefixed with \"+\" to sort ascending or \"-\" to sort descending. If no prefix is specified, the column is sorted ascending.",
       "pattern": "(\\+|-)?[a-zA-Z_]+",
       "repeated": true,
       "location": "query"
      },
      "startDate": {
       "type": "string",
       "description": "Start of the date range to report on in \"YYYY-MM-DD\" format, inclusive.",
       "required": true,
       "pattern": "\\d{4}-\\d{2}-\\d{2}|(today|startOfMonth|startOfYear)(([\\-\\+]\\d+[dwmy]){0,2}?)",
       "location": "query"
      },
      "startIndex": {
       "type": "integer",
       "description": "Index of the first row of report data to return.",
       "format": "int32",
       "minimum": "0",
       "maximum": "5000",
       "location": "query"
      }
     },
     "parameterOrder": [
      "startDate",
      "endDate"
     ],
     "response": {
      "$ref": "AdsenseReportsGenerateResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsense",
      "https://www.googleapis.com/auth/adsense.readonly"
     ],
     "supportsMediaDownload": true
    }
   }
  },
  "urlchannels": {
   "methods": {
    "list": {
     "id": "adsense.urlchannels.list",
     "path": "adclients/{adClientId}/urlchannels",
     "httpMethod": "GET",
     "description": "List all URL channels in the specified ad client for this AdSense account.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client for which to list URL channels.",
       "required": true,
       "location": "path"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of URL channels to include in the response, used for paging.",
       "format": "int32",
       "minimum": "0",
       "maximum": "10000",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token, used to page through URL channels. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "adClientId"
     ],
     "response": {
      "$ref": "UrlChannels"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsense",
      "https://www.googleapis.com/auth/adsense.readonly"
     ]
    }
   }
  }
 }
}
