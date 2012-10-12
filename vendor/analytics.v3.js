{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "analytics:v3",
 "name": "analytics",
 "version": "v3",
 "revision": "20120831",
 "title": "Google Analytics API",
 "description": "View and manage your Google Analytics data",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/analytics-16.png",
  "x32": "http://www.google.com/images/icons/product/analytics-32.png"
 },
 "documentationLink": "https://developers.google.com/analytics/",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/analytics/v3/",
 "basePath": "/analytics/v3/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "analytics/v3/",
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
   "default": "false",
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
    "https://www.googleapis.com/auth/analytics": {
     "description": "View and manage your Google Analytics data"
    },
    "https://www.googleapis.com/auth/analytics.readonly": {
     "description": "View your Google Analytics data"
    }
   }
  }
 },
 "schemas": {
  "Account": {
   "id": "Account",
   "type": "object",
   "description": "JSON template for Analytics account entry.",
   "properties": {
    "childLink": {
     "type": "object",
     "description": "Child link for an account entry. Points to the list of web properties for this account.",
     "properties": {
      "href": {
       "type": "string",
       "description": "Link to the list of web properties for this account."
      },
      "type": {
       "type": "string",
       "description": "Type of the child link. Its value is \"analytics#webproperties\".",
       "default": "analytics#webproperties"
      }
     }
    },
    "created": {
     "type": "string",
     "description": "Time the account was created.",
     "format": "date-time"
    },
    "id": {
     "type": "string",
     "description": "Account ID."
    },
    "kind": {
     "type": "string",
     "description": "Resource type for Analytics account.",
     "default": "analytics#account"
    },
    "name": {
     "type": "string",
     "description": "Account name."
    },
    "selfLink": {
     "type": "string",
     "description": "Link for this account."
    },
    "updated": {
     "type": "string",
     "description": "Time the account was last modified.",
     "format": "date-time"
    }
   }
  },
  "Accounts": {
   "id": "Accounts",
   "type": "object",
   "description": "An account collection provides a list of Analytics accounts to which a user has access. The account collection is the entry point to all management information. Each resource in the collection corresponds to a single Analytics account.",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of accounts.",
     "items": {
      "$ref": "Account"
     }
    },
    "itemsPerPage": {
     "type": "integer",
     "description": "The maximum number of entries the response can contain, regardless of the actual number of entries returned. Its value ranges from 1 to 1000 with a value of 1000 by default, or otherwise specified by the max-results query parameter.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Collection type.",
     "default": "analytics#accounts"
    },
    "nextLink": {
     "type": "string",
     "description": "Next link for this account collection."
    },
    "previousLink": {
     "type": "string",
     "description": "Previous link for this account collection."
    },
    "startIndex": {
     "type": "integer",
     "description": "The starting index of the entries, which is 1 by default or otherwise specified by the start-index query parameter.",
     "format": "int32"
    },
    "totalResults": {
     "type": "integer",
     "description": "The total number of results for the query, regardless of the number of results in the response.",
     "format": "int32"
    },
    "username": {
     "type": "string",
     "description": "Email ID of the authenticated user"
    }
   }
  },
  "GaData": {
   "id": "GaData",
   "type": "object",
   "description": "Analytics data for a given profile.",
   "properties": {
    "columnHeaders": {
     "type": "array",
     "description": "Column headers that list dimension names followed by the metric names. The order of dimensions and metrics is same as specified in the request.",
     "items": {
      "type": "object",
      "properties": {
       "columnType": {
        "type": "string",
        "description": "Column Type. Either DIMENSION or METRIC."
       },
       "dataType": {
        "type": "string",
        "description": "Data type. Dimension column headers have only STRING as the data type. Metric column headers have data types for metric values such as INTEGER, DOUBLE, CURRENCY etc."
       },
       "name": {
        "type": "string",
        "description": "Column name."
       }
      }
     }
    },
    "containsSampledData": {
     "type": "boolean",
     "description": "Determines if Analytics data contains samples."
    },
    "id": {
     "type": "string",
     "description": "Unique ID for this data response."
    },
    "itemsPerPage": {
     "type": "integer",
     "description": "The maximum number of rows the response can contain, regardless of the actual number of rows returned. Its value ranges from 1 to 10,000 with a value of 1000 by default, or otherwise specified by the max-results query parameter.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "analytics#gaData"
    },
    "nextLink": {
     "type": "string",
     "description": "Link to next page for this Analytics data query."
    },
    "previousLink": {
     "type": "string",
     "description": "Link to previous page for this Analytics data query."
    },
    "profileInfo": {
     "type": "object",
     "description": "Information for the profile, for which the Analytics data was requested.",
     "properties": {
      "accountId": {
       "type": "string",
       "description": "Account ID to which this profile belongs."
      },
      "internalWebPropertyId": {
       "type": "string",
       "description": "Internal ID for the web property to which this profile belongs."
      },
      "profileId": {
       "type": "string",
       "description": "Profile ID."
      },
      "profileName": {
       "type": "string",
       "description": "Profile name."
      },
      "tableId": {
       "type": "string",
       "description": "Table ID for profile."
      },
      "webPropertyId": {
       "type": "string",
       "description": "Web Property ID to which this profile belongs."
      }
     }
    },
    "query": {
     "type": "object",
     "description": "Analytics data request query parameters.",
     "properties": {
      "dimensions": {
       "type": "string",
       "description": "List of analytics dimensions."
      },
      "end-date": {
       "type": "string",
       "description": "End date."
      },
      "filters": {
       "type": "string",
       "description": "Comma-separated list of dimension or metric filters."
      },
      "ids": {
       "type": "string",
       "description": "Unique table ID."
      },
      "max-results": {
       "type": "integer",
       "description": "Maximum results per page.",
       "format": "int32"
      },
      "metrics": {
       "type": "array",
       "description": "List of analytics metrics.",
       "items": {
        "type": "string"
       }
      },
      "segment": {
       "type": "string",
       "description": "Analytics advanced segment."
      },
      "sort": {
       "type": "array",
       "description": "List of dimensions or metrics based on which Analytics data is sorted.",
       "items": {
        "type": "string"
       }
      },
      "start-date": {
       "type": "string",
       "description": "Start date."
      },
      "start-index": {
       "type": "integer",
       "description": "Start index.",
       "format": "int32"
      }
     }
    },
    "rows": {
     "type": "array",
     "description": "Analytics data rows, where each row contains a list of dimension values followed by the metric values. The order of dimensions and metrics is same as specified in the request.",
     "items": {
      "type": "array",
      "items": {
       "type": "string"
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "Link to this page."
    },
    "totalResults": {
     "type": "integer",
     "description": "The total number of rows for the query, regardless of the number of rows in the response.",
     "format": "int32"
    },
    "totalsForAllResults": {
     "type": "object",
     "description": "Total values for the requested metrics over all the results, not just the results returned in this response. The order of the metric totals is same as the metric order specified in the request.",
     "additionalProperties": {
      "type": "string",
      "description": "Key-value pair for the total value of a metric. Key is the metric name and the value is the total value for that metric."
     }
    }
   }
  },
  "Goal": {
   "id": "Goal",
   "type": "object",
   "description": "JSON template for Analytics goal resource.",
   "properties": {
    "accountId": {
     "type": "string",
     "description": "Account ID to which this goal belongs."
    },
    "active": {
     "type": "boolean",
     "description": "Determines whether this goal is active."
    },
    "created": {
     "type": "string",
     "description": "Time this goal was created.",
     "format": "date-time"
    },
    "eventDetails": {
     "type": "object",
     "description": "Details for the goal of the type EVENT.",
     "properties": {
      "eventConditions": {
       "type": "array",
       "description": "List of event conditions.",
       "items": {
        "type": "object",
        "properties": {
         "comparisonType": {
          "type": "string",
          "description": "Type of comparison. Possible values are LESS_THAN, GREATER_THAN or EQUAL."
         },
         "comparisonValue": {
          "type": "string",
          "description": "Value used for this comparison.",
          "format": "int64"
         },
         "expression": {
          "type": "string",
          "description": "Expression used for this match."
         },
         "matchType": {
          "type": "string",
          "description": "Type of the match to be performed. Possible values are REGEXP, BEGINS_WITH, or EXACT."
         },
         "type": {
          "type": "string",
          "description": "Type of this event condition. Possible values are CATEGORY, ACTION, LABEL, or VALUE."
         }
        }
       }
      },
      "useEventValue": {
       "type": "boolean",
       "description": "Determines if the event value should be used as the value for this goal."
      }
     }
    },
    "id": {
     "type": "string",
     "description": "Goal ID."
    },
    "internalWebPropertyId": {
     "type": "string",
     "description": "Internal ID for the web property to which this goal belongs."
    },
    "kind": {
     "type": "string",
     "description": "Resource type for an Analytics goal.",
     "default": "analytics#goal"
    },
    "name": {
     "type": "string",
     "description": "Goal name."
    },
    "parentLink": {
     "type": "object",
     "description": "Parent link for a goal. Points to the profile to which this goal belongs.",
     "properties": {
      "href": {
       "type": "string",
       "description": "Link to the profile to which this goal belongs."
      },
      "type": {
       "type": "string",
       "description": "Value is \"analytics#profile\".",
       "default": "analytics#profile"
      }
     }
    },
    "profileId": {
     "type": "string",
     "description": "Profile ID to which this goal belongs."
    },
    "selfLink": {
     "type": "string",
     "description": "Link for this goal."
    },
    "type": {
     "type": "string",
     "description": "Goal type. Possible values are URL_DESTINATION, VISIT_TIME_ON_SITE, VISIT_NUM_PAGES, AND EVENT."
    },
    "updated": {
     "type": "string",
     "description": "Time this goal was last modified.",
     "format": "date-time"
    },
    "urlDestinationDetails": {
     "type": "object",
     "description": "Details for the goal of the type URL_DESTINATION.",
     "properties": {
      "caseSensitive": {
       "type": "boolean",
       "description": "Determines if the goal URL must exactly match the capitalization of visited URLs."
      },
      "firstStepRequired": {
       "type": "boolean",
       "description": "Determines if the first step in this goal is required."
      },
      "matchType": {
       "type": "string",
       "description": "Match type for the goal URL. Possible values are HEAD, EXACT, or REGEX."
      },
      "steps": {
       "type": "array",
       "description": "List of steps configured for this goal funnel.",
       "items": {
        "type": "object",
        "properties": {
         "name": {
          "type": "string",
          "description": "Step name."
         },
         "number": {
          "type": "integer",
          "description": "Step number.",
          "format": "int32"
         },
         "url": {
          "type": "string",
          "description": "URL for this step."
         }
        }
       }
      },
      "url": {
       "type": "string",
       "description": "URL for this goal."
      }
     }
    },
    "value": {
     "type": "number",
     "description": "Goal value.",
     "format": "float"
    },
    "visitNumPagesDetails": {
     "type": "object",
     "description": "Details for the goal of the type VISIT_NUM_PAGES.",
     "properties": {
      "comparisonType": {
       "type": "string",
       "description": "Type of comparison. Possible values are LESS_THAN, GREATER_THAN, or EQUAL."
      },
      "comparisonValue": {
       "type": "string",
       "description": "Value used for this comparison.",
       "format": "int64"
      }
     }
    },
    "visitTimeOnSiteDetails": {
     "type": "object",
     "description": "Details for the goal of the type VISIT_TIME_ON_SITE.",
     "properties": {
      "comparisonType": {
       "type": "string",
       "description": "Type of comparison. Possible values are LESS_THAN or GREATER_THAN."
      },
      "comparisonValue": {
       "type": "string",
       "description": "Value used for this comparison.",
       "format": "int64"
      }
     }
    },
    "webPropertyId": {
     "type": "string",
     "description": "Web property ID to which this goal belongs. The web property ID is of the form UA-XXXXX-YY."
    }
   }
  },
  "Goals": {
   "id": "Goals",
   "type": "object",
   "description": "A goal collection lists Analytics goals to which the user has access. Each profile can have a set of goals. Each resource in the Goal collection corresponds to a single Analytics goal.",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of goals.",
     "items": {
      "$ref": "Goal"
     }
    },
    "itemsPerPage": {
     "type": "integer",
     "description": "The maximum number of resources the response can contain, regardless of the actual number of resources returned. Its value ranges from 1 to 1000 with a value of 1000 by default, or otherwise specified by the max-results query parameter.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Collection type.",
     "default": "analytics#goals"
    },
    "nextLink": {
     "type": "string",
     "description": "Link to next page for this goal collection."
    },
    "previousLink": {
     "type": "string",
     "description": "Link to previous page for this goal collection."
    },
    "startIndex": {
     "type": "integer",
     "description": "The starting index of the resources, which is 1 by default or otherwise specified by the start-index query parameter.",
     "format": "int32"
    },
    "totalResults": {
     "type": "integer",
     "description": "The total number of results for the query, regardless of the number of resources in the result.",
     "format": "int32"
    },
    "username": {
     "type": "string",
     "description": "Email ID of the authenticated user"
    }
   }
  },
  "McfData": {
   "id": "McfData",
   "type": "object",
   "description": "Multi-Channel Funnels data for a given profile.",
   "properties": {
    "columnHeaders": {
     "type": "array",
     "description": "Column headers that list dimension names followed by the metric names. The order of dimensions and metrics is same as specified in the request.",
     "items": {
      "type": "object",
      "properties": {
       "columnType": {
        "type": "string",
        "description": "Column Type. Either DIMENSION or METRIC."
       },
       "dataType": {
        "type": "string",
        "description": "Data type. Dimension and metric values data types such as INTEGER, DOUBLE, CURRENCY, MCF_SEQUENCE etc."
       },
       "name": {
        "type": "string",
        "description": "Column name."
       }
      }
     }
    },
    "containsSampledData": {
     "type": "boolean",
     "description": "Determines if the Analytics data contains sampled data."
    },
    "id": {
     "type": "string",
     "description": "Unique ID for this data response."
    },
    "itemsPerPage": {
     "type": "integer",
     "description": "The maximum number of rows the response can contain, regardless of the actual number of rows returned. Its value ranges from 1 to 10,000 with a value of 1000 by default, or otherwise specified by the max-results query parameter.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "analytics#mcfData"
    },
    "nextLink": {
     "type": "string",
     "description": "Link to next page for this Analytics data query."
    },
    "previousLink": {
     "type": "string",
     "description": "Link to previous page for this Analytics data query."
    },
    "profileInfo": {
     "type": "object",
     "description": "Information for the profile, for which the Analytics data was requested.",
     "properties": {
      "accountId": {
       "type": "string",
       "description": "Account ID to which this profile belongs."
      },
      "internalWebPropertyId": {
       "type": "string",
       "description": "Internal ID for the web property to which this profile belongs."
      },
      "profileId": {
       "type": "string",
       "description": "Profile ID."
      },
      "profileName": {
       "type": "string",
       "description": "Profile name."
      },
      "tableId": {
       "type": "string",
       "description": "Table ID for profile."
      },
      "webPropertyId": {
       "type": "string",
       "description": "Web Property ID to which this profile belongs."
      }
     }
    },
    "query": {
     "type": "object",
     "description": "Analytics data request query parameters.",
     "properties": {
      "dimensions": {
       "type": "string",
       "description": "List of analytics dimensions."
      },
      "end-date": {
       "type": "string",
       "description": "End date."
      },
      "filters": {
       "type": "string",
       "description": "Comma-separated list of dimension or metric filters."
      },
      "ids": {
       "type": "string",
       "description": "Unique table ID."
      },
      "max-results": {
       "type": "integer",
       "description": "Maximum results per page.",
       "format": "int32"
      },
      "metrics": {
       "type": "array",
       "description": "List of analytics metrics.",
       "items": {
        "type": "string"
       }
      },
      "segment": {
       "type": "string",
       "description": "Analytics advanced segment."
      },
      "sort": {
       "type": "array",
       "description": "List of dimensions or metrics based on which Analytics data is sorted.",
       "items": {
        "type": "string"
       }
      },
      "start-date": {
       "type": "string",
       "description": "Start date."
      },
      "start-index": {
       "type": "integer",
       "description": "Start index.",
       "format": "int32"
      }
     }
    },
    "rows": {
     "type": "array",
     "description": "Analytics data rows, where each row contains a list of dimension values followed by the metric values. The order of dimensions and metrics is same as specified in the request.",
     "items": {
      "type": "array",
      "items": {
       "type": "object",
       "description": "A union object representing a dimension or metric value. Only one of \"primitiveValue\" or \"conversionPathValue\" attribute will be populated.",
       "properties": {
        "conversionPathValue": {
         "type": "array",
         "description": "A conversion path dimension value, containing a list of interactions with their attributes.",
         "items": {
          "type": "object",
          "properties": {
           "interactionType": {
            "type": "string",
            "description": "Type of an interaction on conversion path. Such as CLICK, IMPRESSION etc."
           },
           "nodeValue": {
            "type": "string",
            "description": "Node value of an interaction on conversion path. Such as source, medium etc."
           }
          }
         }
        },
        "primitiveValue": {
         "type": "string",
         "description": "A primitive dimension value. A primitive metric value."
        }
       }
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "Link to this page."
    },
    "totalResults": {
     "type": "integer",
     "description": "The total number of rows for the query, regardless of the number of rows in the response.",
     "format": "int32"
    },
    "totalsForAllResults": {
     "type": "object",
     "description": "Total values for the requested metrics over all the results, not just the results returned in this response. The order of the metric totals is same as the metric order specified in the request.",
     "additionalProperties": {
      "type": "string",
      "description": "Key-value pair for the total value of a metric. Key is the metric name and the value is the total value for that metric."
     }
    }
   }
  },
  "Profile": {
   "id": "Profile",
   "type": "object",
   "description": "JSON template for an Analytics profile.",
   "properties": {
    "accountId": {
     "type": "string",
     "description": "Account ID to which this profile belongs."
    },
    "childLink": {
     "type": "object",
     "description": "Child link for this profile. Points to the list of goals for this profile.",
     "properties": {
      "href": {
       "type": "string",
       "description": "Link to the list of goals for this profile."
      },
      "type": {
       "type": "string",
       "description": "Value is \"analytics#goals\".",
       "default": "analytics#goals"
      }
     }
    },
    "created": {
     "type": "string",
     "description": "Time this profile was created.",
     "format": "date-time"
    },
    "currency": {
     "type": "string",
     "description": "The currency type associated with this profile."
    },
    "defaultPage": {
     "type": "string",
     "description": "Default page for this profile."
    },
    "eCommerceTracking": {
     "type": "boolean",
     "description": "E-commerce tracking parameter for this profile."
    },
    "excludeQueryParameters": {
     "type": "string",
     "description": "The query parameters that are excluded from this profile."
    },
    "id": {
     "type": "string",
     "description": "Profile ID."
    },
    "internalWebPropertyId": {
     "type": "string",
     "description": "Internal ID for the web property to which this profile belongs."
    },
    "kind": {
     "type": "string",
     "description": "Resource type for Analytics profile.",
     "default": "analytics#profile"
    },
    "name": {
     "type": "string",
     "description": "Name of this profile."
    },
    "parentLink": {
     "type": "object",
     "description": "Parent link for this profile. Points to the web property to which this profile belongs.",
     "properties": {
      "href": {
       "type": "string",
       "description": "Link to the web property to which this profile belongs."
      },
      "type": {
       "type": "string",
       "description": "Value is \"analytics#webproperty\".",
       "default": "analytics#webproperty"
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "Link for this profile."
    },
    "siteSearchCategoryParameters": {
     "type": "string",
     "description": "Site search category parameters for this profile."
    },
    "siteSearchQueryParameters": {
     "type": "string",
     "description": "The site search query parameters for this profile."
    },
    "timezone": {
     "type": "string",
     "description": "Time zone for which this profile has been configured."
    },
    "updated": {
     "type": "string",
     "description": "Time this profile was last modified.",
     "format": "date-time"
    },
    "webPropertyId": {
     "type": "string",
     "description": "Web property ID of the form UA-XXXXX-YY to which this profile belongs."
    },
    "websiteUrl": {
     "type": "string",
     "description": "Website URL for this profile."
    }
   }
  },
  "Profiles": {
   "id": "Profiles",
   "type": "object",
   "description": "A profile collection lists Analytics profiles to which the user has access. Each resource in the collection corresponds to a single Analytics profile.",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of profiles.",
     "items": {
      "$ref": "Profile"
     }
    },
    "itemsPerPage": {
     "type": "integer",
     "description": "The maximum number of resources the response can contain, regardless of the actual number of resources returned. Its value ranges from 1 to 1000 with a value of 1000 by default, or otherwise specified by the max-results query parameter.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Collection type.",
     "default": "analytics#profiles"
    },
    "nextLink": {
     "type": "string",
     "description": "Link to next page for this profile collection."
    },
    "previousLink": {
     "type": "string",
     "description": "Link to previous page for this profile collection."
    },
    "startIndex": {
     "type": "integer",
     "description": "The starting index of the resources, which is 1 by default or otherwise specified by the start-index query parameter.",
     "format": "int32"
    },
    "totalResults": {
     "type": "integer",
     "description": "The total number of results for the query, regardless of the number of results in the response.",
     "format": "int32"
    },
    "username": {
     "type": "string",
     "description": "Email ID of the authenticated user"
    }
   }
  },
  "Segment": {
   "id": "Segment",
   "type": "object",
   "description": "JSON template for an Analytics advanced segment.",
   "properties": {
    "created": {
     "type": "string",
     "description": "Time the advanced segment was created.",
     "format": "date-time"
    },
    "definition": {
     "type": "string",
     "description": "Advanced segment definition."
    },
    "id": {
     "type": "string",
     "description": "Advanced segment ID."
    },
    "kind": {
     "type": "string",
     "description": "Resource type for Analytics advanced segment.",
     "default": "analytics#segment"
    },
    "name": {
     "type": "string",
     "description": "Advanced segment name."
    },
    "segmentId": {
     "type": "string",
     "description": "Segment ID. Can be used with the 'segment' parameter in Data Feed."
    },
    "selfLink": {
     "type": "string",
     "description": "Link for this advanced segment."
    },
    "updated": {
     "type": "string",
     "description": "Time the advanced segment was last modified.",
     "format": "date-time"
    }
   }
  },
  "Segments": {
   "id": "Segments",
   "type": "object",
   "description": "An advanced segment collection lists Analytics advanced segments that the user has access to. Each resource in the collection corresponds to a single Analytics advanced segment.",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of advanced segments.",
     "items": {
      "$ref": "Segment"
     }
    },
    "itemsPerPage": {
     "type": "integer",
     "description": "The maximum number of resources the response can contain, regardless of the actual number of resources returned. Its value ranges from 1 to 1000 with a value of 1000 by default, or otherwise specified by the max-results query parameter.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Collection type for advanced segments.",
     "default": "analytics#segments"
    },
    "nextLink": {
     "type": "string",
     "description": "Link to next page for this advanced segment collection."
    },
    "previousLink": {
     "type": "string",
     "description": "Link to previous page for this advanced segment collection."
    },
    "startIndex": {
     "type": "integer",
     "description": "The starting index of the resources, which is 1 by default or otherwise specified by the start-index query parameter.",
     "format": "int32"
    },
    "totalResults": {
     "type": "integer",
     "description": "The total number of results for the query, regardless of the number of results in the response.",
     "format": "int32"
    },
    "username": {
     "type": "string",
     "description": "Email ID of the authenticated user"
    }
   }
  },
  "Webproperties": {
   "id": "Webproperties",
   "type": "object",
   "description": "A web property collection lists Analytics web properties to which the user has access. Each resource in the collection corresponds to a single Analytics web property.",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of web properties.",
     "items": {
      "$ref": "Webproperty"
     }
    },
    "itemsPerPage": {
     "type": "integer",
     "description": "The maximum number of resources the response can contain, regardless of the actual number of resources returned. Its value ranges from 1 to 1000 with a value of 1000 by default, or otherwise specified by the max-results query parameter.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Collection type.",
     "default": "analytics#webproperties"
    },
    "nextLink": {
     "type": "string",
     "description": "Link to next page for this web property collection."
    },
    "previousLink": {
     "type": "string",
     "description": "Link to previous page for this web property collection."
    },
    "startIndex": {
     "type": "integer",
     "description": "The starting index of the resources, which is 1 by default or otherwise specified by the start-index query parameter.",
     "format": "int32"
    },
    "totalResults": {
     "type": "integer",
     "description": "The total number of results for the query, regardless of the number of results in the response.",
     "format": "int32"
    },
    "username": {
     "type": "string",
     "description": "Email ID of the authenticated user"
    }
   }
  },
  "Webproperty": {
   "id": "Webproperty",
   "type": "object",
   "description": "JSON template for an Analytics web property.",
   "properties": {
    "accountId": {
     "type": "string",
     "description": "Account ID to which this web property belongs."
    },
    "childLink": {
     "type": "object",
     "description": "Child link for this web property. Points to the list of profiles for this web property.",
     "properties": {
      "href": {
       "type": "string",
       "description": "Link to the list of profiles for this web property."
      },
      "type": {
       "type": "string",
       "description": "Type of the parent link. Its value is \"analytics#profiles\".",
       "default": "analytics#profiles"
      }
     }
    },
    "created": {
     "type": "string",
     "description": "Time this web property was created.",
     "format": "date-time"
    },
    "id": {
     "type": "string",
     "description": "Web property ID of the form UA-XXXXX-YY."
    },
    "internalWebPropertyId": {
     "type": "string",
     "description": "Internal ID for this web property."
    },
    "kind": {
     "type": "string",
     "description": "Resource type for Analytics WebProperty.",
     "default": "analytics#webproperty"
    },
    "name": {
     "type": "string",
     "description": "Name of this web property."
    },
    "parentLink": {
     "type": "object",
     "description": "Parent link for this web property. Points to the account to which this web property belongs.",
     "properties": {
      "href": {
       "type": "string",
       "description": "Link to the account for this web property."
      },
      "type": {
       "type": "string",
       "description": "Type of the parent link. Its value is \"analytics#account\".",
       "default": "analytics#account"
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "Link for this web property."
    },
    "updated": {
     "type": "string",
     "description": "Time this web property was last modified.",
     "format": "date-time"
    },
    "websiteUrl": {
     "type": "string",
     "description": "Website url for this web property."
    }
   }
  }
 },
 "resources": {
  "data": {
   "resources": {
    "ga": {
     "methods": {
      "get": {
       "id": "analytics.data.ga.get",
       "path": "data/ga",
       "httpMethod": "GET",
       "description": "Returns Analytics data for a profile.",
       "parameters": {
        "dimensions": {
         "type": "string",
         "description": "A comma-separated list of Analytics dimensions. E.g., 'ga:browser,ga:city'.",
         "pattern": "(ga:.+)?",
         "location": "query"
        },
        "end-date": {
         "type": "string",
         "description": "End date for fetching Analytics data. All requests should specify an end date formatted as YYYY-MM-DD.",
         "required": true,
         "pattern": "[0-9]{4}-[0-9]{2}-[0-9]{2}",
         "location": "query"
        },
        "filters": {
         "type": "string",
         "description": "A comma-separated list of dimension or metric filters to be applied to Analytics data.",
         "pattern": "ga:.+",
         "location": "query"
        },
        "ids": {
         "type": "string",
         "description": "Unique table ID for retrieving Analytics data. Table ID is of the form ga:XXXX, where XXXX is the Analytics profile ID.",
         "required": true,
         "pattern": "ga:[0-9]+",
         "location": "query"
        },
        "max-results": {
         "type": "integer",
         "description": "The maximum number of entries to include in this feed.",
         "format": "int32",
         "location": "query"
        },
        "metrics": {
         "type": "string",
         "description": "A comma-separated list of Analytics metrics. E.g., 'ga:visits,ga:pageviews'. At least one metric must be specified.",
         "required": true,
         "pattern": "ga:.+",
         "location": "query"
        },
        "segment": {
         "type": "string",
         "description": "An Analytics advanced segment to be applied to data.",
         "location": "query"
        },
        "sort": {
         "type": "string",
         "description": "A comma-separated list of dimensions or metrics that determine the sort order for Analytics data.",
         "pattern": "(-)?ga:.+",
         "location": "query"
        },
        "start-date": {
         "type": "string",
         "description": "Start date for fetching Analytics data. All requests should specify a start date formatted as YYYY-MM-DD.",
         "required": true,
         "pattern": "[0-9]{4}-[0-9]{2}-[0-9]{2}",
         "location": "query"
        },
        "start-index": {
         "type": "integer",
         "description": "An index of the first entity to retrieve. Use this parameter as a pagination mechanism along with the max-results parameter.",
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
        "$ref": "GaData"
       },
       "scopes": [
        "https://www.googleapis.com/auth/analytics",
        "https://www.googleapis.com/auth/analytics.readonly"
       ]
      }
     }
    },
    "mcf": {
     "methods": {
      "get": {
       "id": "analytics.data.mcf.get",
       "path": "data/mcf",
       "httpMethod": "GET",
       "description": "Returns Analytics Multi-Channel Funnels data for a profile.",
       "parameters": {
        "dimensions": {
         "type": "string",
         "description": "A comma-separated list of Multi-Channel Funnels dimensions. E.g., 'mcf:source,mcf:medium'.",
         "pattern": "(mcf:.+)?",
         "location": "query"
        },
        "end-date": {
         "type": "string",
         "description": "End date for fetching Analytics data. All requests should specify an end date formatted as YYYY-MM-DD.",
         "required": true,
         "pattern": "[0-9]{4}-[0-9]{2}-[0-9]{2}",
         "location": "query"
        },
        "filters": {
         "type": "string",
         "description": "A comma-separated list of dimension or metric filters to be applied to the Analytics data.",
         "pattern": "mcf:.+",
         "location": "query"
        },
        "ids": {
         "type": "string",
         "description": "Unique table ID for retrieving Analytics data. Table ID is of the form ga:XXXX, where XXXX is the Analytics profile ID.",
         "required": true,
         "pattern": "ga:[0-9]+",
         "location": "query"
        },
        "max-results": {
         "type": "integer",
         "description": "The maximum number of entries to include in this feed.",
         "format": "int32",
         "location": "query"
        },
        "metrics": {
         "type": "string",
         "description": "A comma-separated list of Multi-Channel Funnels metrics. E.g., 'mcf:totalConversions,mcf:totalConversionValue'. At least one metric must be specified.",
         "required": true,
         "pattern": "mcf:.+",
         "location": "query"
        },
        "sort": {
         "type": "string",
         "description": "A comma-separated list of dimensions or metrics that determine the sort order for the Analytics data.",
         "pattern": "(-)?mcf:.+",
         "location": "query"
        },
        "start-date": {
         "type": "string",
         "description": "Start date for fetching Analytics data. All requests should specify a start date formatted as YYYY-MM-DD.",
         "required": true,
         "pattern": "[0-9]{4}-[0-9]{2}-[0-9]{2}",
         "location": "query"
        },
        "start-index": {
         "type": "integer",
         "description": "An index of the first entity to retrieve. Use this parameter as a pagination mechanism along with the max-results parameter.",
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
        "$ref": "McfData"
       },
       "scopes": [
        "https://www.googleapis.com/auth/analytics",
        "https://www.googleapis.com/auth/analytics.readonly"
       ]
      }
     }
    }
   }
  },
  "management": {
   "resources": {
    "accounts": {
     "methods": {
      "list": {
       "id": "analytics.management.accounts.list",
       "path": "management/accounts",
       "httpMethod": "GET",
       "description": "Lists all accounts to which the user has access.",
       "parameters": {
        "max-results": {
         "type": "integer",
         "description": "The maximum number of accounts to include in this response.",
         "format": "int32",
         "location": "query"
        },
        "start-index": {
         "type": "integer",
         "description": "An index of the first account to retrieve. Use this parameter as a pagination mechanism along with the max-results parameter.",
         "format": "int32",
         "minimum": "1",
         "location": "query"
        }
       },
       "response": {
        "$ref": "Accounts"
       },
       "scopes": [
        "https://www.googleapis.com/auth/analytics",
        "https://www.googleapis.com/auth/analytics.readonly"
       ]
      }
     }
    },
    "goals": {
     "methods": {
      "list": {
       "id": "analytics.management.goals.list",
       "path": "management/accounts/{accountId}/webproperties/{webPropertyId}/profiles/{profileId}/goals",
       "httpMethod": "GET",
       "description": "Lists goals to which the user has access.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account ID to retrieve goals for. Can either be a specific account ID or '~all', which refers to all the accounts that user has access to.",
         "required": true,
         "location": "path"
        },
        "max-results": {
         "type": "integer",
         "description": "The maximum number of goals to include in this response.",
         "format": "int32",
         "location": "query"
        },
        "profileId": {
         "type": "string",
         "description": "Profile ID to retrieve goals for. Can either be a specific profile ID or '~all', which refers to all the profiles that user has access to.",
         "required": true,
         "location": "path"
        },
        "start-index": {
         "type": "integer",
         "description": "An index of the first goal to retrieve. Use this parameter as a pagination mechanism along with the max-results parameter.",
         "format": "int32",
         "minimum": "1",
         "location": "query"
        },
        "webPropertyId": {
         "type": "string",
         "description": "Web property ID to retrieve goals for. Can either be a specific web property ID or '~all', which refers to all the web properties that user has access to.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "accountId",
        "webPropertyId",
        "profileId"
       ],
       "response": {
        "$ref": "Goals"
       },
       "scopes": [
        "https://www.googleapis.com/auth/analytics",
        "https://www.googleapis.com/auth/analytics.readonly"
       ]
      }
     }
    },
    "profiles": {
     "methods": {
      "list": {
       "id": "analytics.management.profiles.list",
       "path": "management/accounts/{accountId}/webproperties/{webPropertyId}/profiles",
       "httpMethod": "GET",
       "description": "Lists profiles to which the user has access.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account ID for the profiles to retrieve. Can either be a specific account ID or '~all', which refers to all the accounts to which the user has access.",
         "required": true,
         "location": "path"
        },
        "max-results": {
         "type": "integer",
         "description": "The maximum number of profiles to include in this response.",
         "format": "int32",
         "location": "query"
        },
        "start-index": {
         "type": "integer",
         "description": "An index of the first entity to retrieve. Use this parameter as a pagination mechanism along with the max-results parameter.",
         "format": "int32",
         "minimum": "1",
         "location": "query"
        },
        "webPropertyId": {
         "type": "string",
         "description": "Web property ID for the profiles to retrieve. Can either be a specific web property ID or '~all', which refers to all the web properties to which the user has access.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "accountId",
        "webPropertyId"
       ],
       "response": {
        "$ref": "Profiles"
       },
       "scopes": [
        "https://www.googleapis.com/auth/analytics",
        "https://www.googleapis.com/auth/analytics.readonly"
       ]
      }
     }
    },
    "segments": {
     "methods": {
      "list": {
       "id": "analytics.management.segments.list",
       "path": "management/segments",
       "httpMethod": "GET",
       "description": "Lists advanced segments to which the user has access.",
       "parameters": {
        "max-results": {
         "type": "integer",
         "description": "The maximum number of advanced segments to include in this response.",
         "format": "int32",
         "location": "query"
        },
        "start-index": {
         "type": "integer",
         "description": "An index of the first advanced segment to retrieve. Use this parameter as a pagination mechanism along with the max-results parameter.",
         "format": "int32",
         "minimum": "1",
         "location": "query"
        }
       },
       "response": {
        "$ref": "Segments"
       },
       "scopes": [
        "https://www.googleapis.com/auth/analytics",
        "https://www.googleapis.com/auth/analytics.readonly"
       ]
      }
     }
    },
    "webproperties": {
     "methods": {
      "list": {
       "id": "analytics.management.webproperties.list",
       "path": "management/accounts/{accountId}/webproperties",
       "httpMethod": "GET",
       "description": "Lists web properties to which the user has access.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account ID to retrieve web properties for. Can either be a specific account ID or '~all', which refers to all the accounts that user has access to.",
         "required": true,
         "location": "path"
        },
        "max-results": {
         "type": "integer",
         "description": "The maximum number of web properties to include in this response.",
         "format": "int32",
         "location": "query"
        },
        "start-index": {
         "type": "integer",
         "description": "An index of the first entity to retrieve. Use this parameter as a pagination mechanism along with the max-results parameter.",
         "format": "int32",
         "minimum": "1",
         "location": "query"
        }
       },
       "parameterOrder": [
        "accountId"
       ],
       "response": {
        "$ref": "Webproperties"
       },
       "scopes": [
        "https://www.googleapis.com/auth/analytics",
        "https://www.googleapis.com/auth/analytics.readonly"
       ]
      }
     }
    }
   }
  }
 }
}
