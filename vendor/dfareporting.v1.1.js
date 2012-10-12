{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "dfareporting:v1.1",
 "name": "dfareporting",
 "version": "v1.1",
 "revision": "20120831",
 "title": "DFA Reporting API",
 "description": "Lets you create, run and download reports.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/doubleclick-16.gif",
  "x32": "http://www.google.com/images/icons/product/doubleclick-32.gif"
 },
 "documentationLink": "https://developers.google.com/doubleclick-advertisers/reporting/",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/dfareporting/v1.1/",
 "basePath": "/dfareporting/v1.1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "dfareporting/v1.1/",
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
    "https://www.googleapis.com/auth/dfareporting": {
     "description": "View and manage DoubleClick for Advertisers reports"
    }
   }
  }
 },
 "schemas": {
  "Activities": {
   "id": "Activities",
   "type": "object",
   "description": "Represents an activity group.",
   "properties": {
    "filters": {
     "type": "array",
     "description": "List of activity filters. The dimension values need to be all either of type \"dfa:activity\" or \"dfa:activityGroup\".",
     "items": {
      "$ref": "DimensionValue"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of resource this is, in this case dfareporting#activities.",
     "default": "dfareporting#activities"
    },
    "metricNames": {
     "type": "array",
     "description": "List of names of floodlight activity metrics.",
     "items": {
      "type": "string"
     }
    }
   }
  },
  "CustomRichMediaEvents": {
   "id": "CustomRichMediaEvents",
   "type": "object",
   "description": "Represents a Custom Rich Media Events group.",
   "properties": {
    "filteredEventIds": {
     "type": "array",
     "description": "List of custom rich media event IDs. Dimension values must be all of type dfa:richMediaEventTypeIdAndName.",
     "items": {
      "$ref": "DimensionValue"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of resource this is, in this case dfareporting#customRichMediaEvents.",
     "default": "dfareporting#customRichMediaEvents"
    }
   }
  },
  "DateRange": {
   "id": "DateRange",
   "type": "object",
   "description": "Represents a date range.",
   "properties": {
    "endDate": {
     "type": "string",
     "description": "The end date of the date range, inclusive. A string of the format: \"yyyy-MM-dd\".",
     "format": "date"
    },
    "kind": {
     "type": "string",
     "description": "The kind of resource this is, in this case dfareporting#dateRange.",
     "default": "dfareporting#dateRange"
    },
    "relativeDateRange": {
     "type": "string",
     "description": "The date range relative to the date of when the report is run, one of:  \n- \"TODAY\" \n- \"YESTERDAY\" \n- \"WEEK_TO_DATE\" \n- \"MONTH_TO_DATE\" \n- \"QUARTER_TO_DATE\" \n- \"YEAR_TO_DATE\" \n- \"PREVIOUS_WEEK\" \n- \"PREVIOUS_MONTH\" \n- \"PREVIOUS_QUARTER\" \n- \"PREVIOUS_YEAR\" \n- \"LAST_7_DAYS\" \n- \"LAST_30_DAYS\" \n- \"LAST_90_DAYS\" \n- \"LAST_365_DAYS\" \n- \"LAST_24_MONTHS\""
    },
    "startDate": {
     "type": "string",
     "description": "The start date of the date range, inclusive. A string of the format: \"yyyy-MM-dd\".",
     "format": "date"
    }
   }
  },
  "DimensionFilter": {
   "id": "DimensionFilter",
   "type": "object",
   "description": "Represents a dimension filter.",
   "properties": {
    "dimensionName": {
     "type": "string",
     "description": "The name of the dimension to filter."
    },
    "kind": {
     "type": "string",
     "description": "The kind of resource this is, in this case dfareporting#dimensionFilter.",
     "default": "dfareporting#dimensionFilter"
    },
    "value": {
     "type": "string",
     "description": "The value of the dimension to filter."
    }
   }
  },
  "DimensionValue": {
   "id": "DimensionValue",
   "type": "object",
   "description": "Represents a DimensionValue resource.",
   "properties": {
    "dimensionName": {
     "type": "string",
     "description": "The name of the dimension."
    },
    "etag": {
     "type": "string",
     "description": "The eTag of this response for caching purposes."
    },
    "id": {
     "type": "string",
     "description": "The ID associated with the value if available."
    },
    "kind": {
     "type": "string",
     "description": "The kind of resource this is, in this case dfareporting#dimensionValue.",
     "default": "dfareporting#dimensionValue"
    },
    "value": {
     "type": "string",
     "description": "The value of the dimension."
    }
   }
  },
  "DimensionValueList": {
   "id": "DimensionValueList",
   "type": "object",
   "description": "Represents the list of DimensionValue resources.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of this response for caching purposes."
    },
    "items": {
     "type": "array",
     "description": "The dimension values returned in this response.",
     "items": {
      "$ref": "DimensionValue"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of list this is, in this case dfareporting#dimensionValueList.",
     "default": "dfareporting#dimensionValueList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through dimension values. To retrieve the next page of results, set the next request's \"pageToken\" to the value of this field. The page token is only valid for a limited amount of time and should not be persisted."
    }
   }
  },
  "DimensionValueRequest": {
   "id": "DimensionValueRequest",
   "type": "object",
   "description": "Represents a DimensionValuesRequest.",
   "properties": {
    "dimensionName": {
     "type": "string",
     "description": "The name of the dimension for which values should be requested."
    },
    "endDate": {
     "type": "string",
     "description": "The end date of the date range for which to retrieve dimension values. A string of the format: \"yyyy-MM-dd\".",
     "format": "date"
    },
    "filters": {
     "type": "array",
     "description": "The list of filters by which to filter values. The filters are ANDed.",
     "items": {
      "$ref": "DimensionFilter"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of request this is, in this case dfareporting#dimensionValueRequest.",
     "default": "dfareporting#dimensionValueRequest"
    },
    "startDate": {
     "type": "string",
     "description": "The start date of the date range for which to retrieve dimension values. A string of the format: \"yyyy-MM-dd\".",
     "format": "date"
    }
   }
  },
  "File": {
   "id": "File",
   "type": "object",
   "description": "Represents a File resource. A File contains the meta-data for a report run. It shows the status of the run and holds the urls to the generated report data if the run is finished and the status is \"REPORT_AVAILABLE\".",
   "properties": {
    "dateRange": {
     "$ref": "DateRange",
     "description": "The date range for which the file has report data. The date range will always be the absolute date range for which the report is run."
    },
    "etag": {
     "type": "string",
     "description": "The eTag of this response for caching purposes."
    },
    "fileName": {
     "type": "string",
     "description": "The file name of the file."
    },
    "format": {
     "type": "string",
     "description": "The output format of the report. Only available once the file is available."
    },
    "id": {
     "type": "string",
     "description": "The unique ID of this report file.",
     "format": "int64"
    },
    "kind": {
     "type": "string",
     "description": "The kind of resource this is, in this case dfareporting#file.",
     "default": "dfareporting#file"
    },
    "lastModifiedTime": {
     "type": "string",
     "description": "The timestamp in milliseconds since epoch when this file was last modified.",
     "format": "int64"
    },
    "reportId": {
     "type": "string",
     "description": "The ID of the report this file was generated from.",
     "format": "int64"
    },
    "status": {
     "type": "string",
     "description": "The status of the report file, one of:  \n- \"PROCESSING\" \n- \"REPORT_AVAILABLE\" \n- \"FAILED\" \n- \"CANCELLED\""
    },
    "urls": {
     "type": "object",
     "description": "The urls where the completed report file can be downloaded.",
     "properties": {
      "apiUrl": {
       "type": "string",
       "description": "The url for downloading the report data through the API."
      },
      "browserUrl": {
       "type": "string",
       "description": "The url for downloading the report data through a browser."
      }
     }
    }
   }
  },
  "FileList": {
   "id": "FileList",
   "type": "object",
   "description": "Represents the list of File resources.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of this response for caching purposes."
    },
    "items": {
     "type": "array",
     "description": "The files returned in this response.",
     "items": {
      "$ref": "File"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of list this is, in this case dfareporting#fileList.",
     "default": "dfareporting#fileList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through files. To retrieve the next page of results, set the next request's \"pageToken\" to the value of this field. The page token is only valid for a limited amount of time and should not be persisted."
    }
   }
  },
  "Recipient": {
   "id": "Recipient",
   "type": "object",
   "description": "Represents a recipient.",
   "properties": {
    "deliveryType": {
     "type": "string",
     "description": "The delivery type for the recipient, one of:  \n- \"ATTACHMENT\"  (support for additional options will be added later)",
     "annotations": {
      "required": [
       "dfareporting.reports.insert",
       "dfareporting.reports.update"
      ]
     }
    },
    "email": {
     "type": "string",
     "description": "The email address of the recipient.",
     "annotations": {
      "required": [
       "dfareporting.reports.insert",
       "dfareporting.reports.update"
      ]
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of resource this is, in this case dfareporting#recipient.",
     "default": "dfareporting#recipient"
    }
   }
  },
  "Report": {
   "id": "Report",
   "type": "object",
   "description": "Represents a Report resource.",
   "properties": {
    "accountId": {
     "type": "string",
     "description": "The account ID to which this report belongs.",
     "format": "int64",
     "annotations": {
      "required": [
       "dfareporting.reports.update"
      ]
     }
    },
    "activeGrpCriteria": {
     "type": "object",
     "description": "The report criteria for a report of type \"ACTIVE_GRP\".",
     "properties": {
      "dateRange": {
       "$ref": "DateRange",
       "description": "The date range this report should be run for."
      },
      "dimensionFilters": {
       "type": "array",
       "description": "The list of filters on which dimensions are filtered.\nFilters for different dimensions are ANDed, filters for the same dimension are grouped together and ORed.\nA valid active GRP report needs to have exactly one DimensionValue for the United States in addition to any advertiser or campaign dimension values.",
       "items": {
        "$ref": "DimensionValue"
       }
      },
      "dimensions": {
       "type": "array",
       "description": "The list of dimensions the report should include.",
       "items": {
        "$ref": "SortedDimension"
       }
      },
      "metricNames": {
       "type": "array",
       "description": "The list of names of metrics the report should include.",
       "items": {
        "type": "string"
       }
      }
     }
    },
    "criteria": {
     "type": "object",
     "description": "The report criteria for a report of type \"STANDARD\".",
     "properties": {
      "activities": {
       "$ref": "Activities",
       "description": "Activity group."
      },
      "customRichMediaEvents": {
       "$ref": "CustomRichMediaEvents",
       "description": "Custom Rich Media Events group."
      },
      "dateRange": {
       "$ref": "DateRange",
       "description": "The date range for which this report should be run."
      },
      "dimensionFilters": {
       "type": "array",
       "description": "The list of filters on which dimensions are filtered.\nFilters for different dimensions are ANDed, filters for the same dimension are grouped together and ORed.",
       "items": {
        "$ref": "DimensionValue"
       }
      },
      "dimensions": {
       "type": "array",
       "description": "The list of standard dimensions the report should include.",
       "items": {
        "$ref": "SortedDimension"
       }
      },
      "metricNames": {
       "type": "array",
       "description": "The list of names of metrics the report should include.",
       "items": {
        "type": "string"
       }
      }
     }
    },
    "crossDimensionReachCriteria": {
     "type": "object",
     "description": "The report criteria for a report of type \"CROSS_DIMENSION_REACH\".",
     "properties": {
      "breakdown": {
       "type": "array",
       "description": "The list of dimensions the report should include.",
       "items": {
        "$ref": "SortedDimension"
       }
      },
      "dateRange": {
       "$ref": "DateRange",
       "description": "The date range this report should be run for."
      },
      "dimension": {
       "type": "string",
       "description": "The dimension option, one of:  \n- \"ADVERTISER\" \n- \"CAMPAIGN\" \n- \"SITE_BY_ADVERTISER\" \n- \"SITE_BY_CAMPAIGN\""
      },
      "dimensionFilters": {
       "type": "array",
       "description": "The list of filters on which dimensions are filtered.",
       "items": {
        "$ref": "DimensionValue"
       }
      },
      "metricNames": {
       "type": "array",
       "description": "The list of names of metrics the report should include.",
       "items": {
        "type": "string"
       }
      },
      "overlapMetricNames": {
       "type": "array",
       "description": "The list of names of overlap metrics the report should include.",
       "items": {
        "type": "string"
       }
      },
      "pivoted": {
       "type": "boolean",
       "description": "Whether the report is pivoted or not. Defaults to true."
      }
     }
    },
    "delivery": {
     "type": "object",
     "description": "The report's email delivery settings.",
     "properties": {
      "emailOwner": {
       "type": "boolean",
       "description": "Whether the report should be emailed to the report owner."
      },
      "emailOwnerDeliveryType": {
       "type": "string",
       "description": "The type of delivery for the owner to receive, if enabled. One of:  \n- \"ATTACHMENT\"  (support for additional options will be added later)"
      },
      "message": {
       "type": "string",
       "description": "The message to be sent with each email."
      },
      "recipients": {
       "type": "array",
       "description": "The list of recipients to which to email the report.",
       "items": {
        "$ref": "Recipient"
       }
      }
     }
    },
    "etag": {
     "type": "string",
     "description": "The eTag of this response for caching purposes."
    },
    "fileName": {
     "type": "string",
     "description": "The file name used when generating report files for this report."
    },
    "floodlightCriteria": {
     "type": "object",
     "description": "The report criteria for a report of type \"FLOODLIGHT\".",
     "properties": {
      "dateRange": {
       "$ref": "DateRange",
       "description": "The date range this report should be run for."
      },
      "dimensionFilters": {
       "type": "array",
       "description": "The list of filters on which dimensions are filtered.\nFilters for different dimensions are ANDed, filters for the same dimension are grouped together and ORed.",
       "items": {
        "$ref": "DimensionValue"
       }
      },
      "dimensions": {
       "type": "array",
       "description": "The list of dimensions the report should include.",
       "items": {
        "$ref": "SortedDimension"
       }
      },
      "floodlightConfigId": {
       "$ref": "DimensionValue",
       "description": "The floodlight ID for which to show data in this report. All advertisers associated with that ID will automatically be added. The dimension of the value needs to be 'dfa:floodlightConfigId'."
      },
      "metricNames": {
       "type": "array",
       "description": "The list of names of metrics the report should include.",
       "items": {
        "type": "string"
       }
      },
      "reportProperties": {
       "type": "object",
       "description": "The properties of the report.",
       "properties": {
        "includeAttributedIPConversions": {
         "type": "boolean",
         "description": "Include conversions that have no cookie, but do have an exposure path."
        },
        "includeUnattributedCookieConversions": {
         "type": "boolean",
         "description": "Include conversions of users with a DoubleClick cookie but without an exposure. That means the user did not click or see an ad from the advertiser within the Floodlight group, or that the interaction happened outside the lookback window."
        },
        "includeUnattributedIPConversions": {
         "type": "boolean",
         "description": "Include conversions that have no associated cookies and no exposures. It’s therefore impossible to know how the user was exposed to your ads during the lookback window prior to a conversion."
        }
       }
      }
     }
    },
    "format": {
     "type": "string",
     "description": "The output format of the report, currently only \"CSV\" is supported. If not specified, default format is \"CSV\". Note that the actual format in the completed report file might differ if for instance the report's size exceeds the format's capabilities. \"CSV\" will then be the fallback format."
    },
    "id": {
     "type": "string",
     "description": "The unique ID identifying this report resource.",
     "format": "int64",
     "annotations": {
      "required": [
       "dfareporting.reports.update"
      ]
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of resource this is, in this case dfareporting#report.",
     "default": "dfareporting#report"
    },
    "lastModifiedTime": {
     "type": "string",
     "description": "The timestamp (in milliseconds since epoch) of when this report was last modified.",
     "format": "uint64",
     "annotations": {
      "required": [
       "dfareporting.reports.update"
      ]
     }
    },
    "name": {
     "type": "string",
     "description": "The name of the report.",
     "annotations": {
      "required": [
       "dfareporting.reports.insert",
       "dfareporting.reports.update"
      ]
     }
    },
    "ownerProfileId": {
     "type": "string",
     "description": "The user profile id of the owner of this report.",
     "format": "int64",
     "annotations": {
      "required": [
       "dfareporting.reports.update"
      ]
     }
    },
    "pathToConversionCriteria": {
     "type": "object",
     "description": "The report criteria for a report of type \"PATH_TO_CONVERSION\".",
     "properties": {
      "activityFilters": {
       "type": "array",
       "description": "The list of 'dfa:activity' values to filter on.",
       "items": {
        "$ref": "DimensionValue"
       }
      },
      "conversionDimensions": {
       "type": "array",
       "description": "The list of conversion dimensions the report should include.",
       "items": {
        "$ref": "SortedDimension"
       }
      },
      "customFloodlightVariables": {
       "type": "array",
       "description": "The list of custom floodlight variables the report should include.",
       "items": {
        "$ref": "SortedDimension"
       }
      },
      "dateRange": {
       "$ref": "DateRange",
       "description": "The date range this report should be run for."
      },
      "floodlightConfigId": {
       "$ref": "DimensionValue",
       "description": "The floodlight ID for which to show data in this report. All advertisers associated with that ID will automatically be added. The dimension of the value needs to be 'dfa:floodlightConfigId'."
      },
      "metricNames": {
       "type": "array",
       "description": "The list of names of metrics the report should include.",
       "items": {
        "type": "string"
       }
      },
      "perInteractionDimensions": {
       "type": "array",
       "description": "The list of per interaction dimensions the report should include.",
       "items": {
        "$ref": "SortedDimension"
       }
      },
      "reportProperties": {
       "type": "object",
       "description": "The properties of the report.",
       "properties": {
        "clicksLookbackWindow": {
         "type": "integer",
         "description": "DFA checks to see if a click interaction occurred within the specified period of time before a conversion. By default the value is pulled from Floodlight or you can manually enter a custom value. Valid values: 1-90.",
         "format": "int32"
        },
        "impressionsLookbackWindow": {
         "type": "integer",
         "description": "DFA checks to see if an impression interaction occurred within the specified period of time before a conversion. By default the value is pulled from Floodlight or you can manually enter a custom value. Valid values: 1-90.",
         "format": "int32"
        },
        "includeAttributedIPConversions": {
         "type": "boolean",
         "description": "Include conversions that have no cookie, but do have an exposure path."
        },
        "includeUnattributedCookieConversions": {
         "type": "boolean",
         "description": "Include conversions of users with a DoubleClick cookie but without an exposure. That means the user did not click or see an ad from the advertiser within the Floodlight group, or that the interaction happened outside the lookback window."
        },
        "includeUnattributedIPConversions": {
         "type": "boolean",
         "description": "Include conversions that have no associated cookies and no exposures. It’s therefore impossible to know how the user was exposed to your ads during the lookback window prior to a conversion."
        },
        "maximumClickInteractions": {
         "type": "integer",
         "description": "The maximum number of click interactions to include in the report. Advertisers currently paying for E2C reports get up to 200 (100 clicks, 100 impressions). If another advertiser in your network is paying for E2C, you can have up to 5 total exposures per report.",
         "format": "int32"
        },
        "maximumImpressionInteractions": {
         "type": "integer",
         "description": "The maximum number of click interactions to include in the report. Advertisers currently paying for E2C reports get up to 200 (100 clicks, 100 impressions). If another advertiser in your network is paying for E2C, you can have up to 5 total exposures per report.",
         "format": "int32"
        },
        "maximumInteractionGap": {
         "type": "integer",
         "description": "The maximum amount of time that can take place between interactions (clicks or impressions) by the same user. Valid values: 1-90.",
         "format": "int32"
        },
        "pivotOnInteractionPath": {
         "type": "boolean",
         "description": "Enable pivoting on interaction path."
        }
       }
      }
     }
    },
    "reachCriteria": {
     "type": "object",
     "description": "The report criteria for a report of type \"REACH\".",
     "properties": {
      "activities": {
       "$ref": "Activities",
       "description": "Activity group."
      },
      "customRichMediaEvents": {
       "$ref": "CustomRichMediaEvents",
       "description": "Custom Rich Media Events group."
      },
      "dateRange": {
       "$ref": "DateRange",
       "description": "The date range this report should be run for."
      },
      "dimensionFilters": {
       "type": "array",
       "description": "The list of filters on which dimensions are filtered.\nFilters for different dimensions are ANDed, filters for the same dimension are grouped together and ORed.",
       "items": {
        "$ref": "DimensionValue"
       }
      },
      "dimensions": {
       "type": "array",
       "description": "The list of dimensions the report should include.",
       "items": {
        "$ref": "SortedDimension"
       }
      },
      "metricNames": {
       "type": "array",
       "description": "The list of names of metrics the report should include.",
       "items": {
        "type": "string"
       }
      },
      "reachByFrequencyMetricNames": {
       "type": "array",
       "description": "The list of names of  Reach By Frequency metrics the report should include.",
       "items": {
        "type": "string"
       }
      }
     }
    },
    "schedule": {
     "type": "object",
     "description": "The report's schedule. Can only be set if the report's 'dateRange' is a relative date range and the relative date range is not \"TODAY\".",
     "properties": {
      "active": {
       "type": "boolean",
       "description": "Whether the schedule is active or not. Must be set to either true or false.",
       "annotations": {
        "required": [
         "dfareporting.reports.insert",
         "dfareporting.reports.update"
        ]
       }
      },
      "every": {
       "type": "integer",
       "description": "Defines every how many days, weeks or months the report should be run. Needs to be set when \"repeats\" is either \"DAILY\", \"WEEKLY\" or \"MONTHLY\".",
       "format": "int32"
      },
      "expirationDate": {
       "type": "string",
       "description": "The expiration date when the scheduled report stops running.",
       "format": "date",
       "annotations": {
        "required": [
         "dfareporting.reports.insert",
         "dfareporting.reports.update"
        ]
       }
      },
      "repeats": {
       "type": "string",
       "description": "The interval for which the report is repeated, one of:  \n- \"DAILY\", also requires field \"every\" to be set. \n- \"WEEKLY\", also requires fields \"every\" and \"repeatsOnWeekDays\" to be set. \n- \"TWICE_A_MONTH\" \n- \"MONTHLY\", also requires fields \"every\" and \"runsOnDayOfMonth\" to be set. \n- \"QUARTERLY\" \n- \"YEARLY\"",
       "annotations": {
        "required": [
         "dfareporting.reports.insert",
         "dfareporting.reports.update"
        ]
       }
      },
      "repeatsOnWeekDays": {
       "type": "array",
       "description": "List of week days \"WEEKLY\" on which scheduled reports should run.",
       "items": {
        "type": "string"
       }
      },
      "runsOnDayOfMonth": {
       "type": "string",
       "description": "Enum to define for \"MONTHLY\" scheduled reports whether reports should be repeated on the same day of the month as \"startDate\" or the same day of the week of the month. Possible values are:  \n- DAY_OF_MONTH \n- WEEK_OF_MONTH  \nExample: If 'startDate' is Monday, April 2nd 2012 (2012-04-02), \"DAY_OF_MONTH\" would run subsequent reports on the 2nd of every Month, and \"WEEK_OF_MONTH\" would run subsequent reports on the first Monday of the month."
      },
      "startDate": {
       "type": "string",
       "description": "Start date of date range for which scheduled reports should be run.",
       "format": "date",
       "annotations": {
        "required": [
         "dfareporting.reports.insert",
         "dfareporting.reports.update"
        ]
       }
      }
     }
    },
    "subAccountId": {
     "type": "string",
     "description": "The subbaccount ID to which this report belongs if applicable.",
     "format": "int64"
    },
    "type": {
     "type": "string",
     "description": "The type of the report, one of:  \n- STANDARD \n- REACH \n- ACTIVE_GRP \n- PATH_TO_CONVERSION \n- FLOODLIGHT \n- CROSS_DIMENSION_REACH",
     "annotations": {
      "required": [
       "dfareporting.reports.insert",
       "dfareporting.reports.update"
      ]
     }
    }
   }
  },
  "ReportList": {
   "id": "ReportList",
   "type": "object",
   "description": "Represents the list of reports.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of this response for caching purposes."
    },
    "items": {
     "type": "array",
     "description": "The reports returned in this response.",
     "items": {
      "$ref": "Report"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of list this is, in this case dfareporting#reportList.",
     "default": "dfareporting#reportList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through reports. To retrieve the next page of results, set the next request's \"pageToken\" to the value of this field. The page token is only valid for a limited amount of time and should not be persisted."
    }
   }
  },
  "SortedDimension": {
   "id": "SortedDimension",
   "type": "object",
   "description": "Represents a sorted dimension.",
   "properties": {
    "kind": {
     "type": "string",
     "description": "The kind of resource this is, in this case dfareporting#sortedDimension.",
     "default": "dfareporting#sortedDimension"
    },
    "name": {
     "type": "string",
     "description": "The name of the dimension."
    },
    "sortOrder": {
     "type": "string",
     "description": "An optional sort order for the dimension column, one of:  \n- \"ASCENDING\" \n- \"DESCENDING\""
    }
   }
  },
  "UserProfile": {
   "id": "UserProfile",
   "type": "object",
   "description": "Represents a UserProfile resource.",
   "properties": {
    "accountId": {
     "type": "string",
     "description": "The account ID to which this profile belongs.",
     "format": "int64"
    },
    "accountName": {
     "type": "string",
     "description": "The account name this profile belongs to."
    },
    "etag": {
     "type": "string",
     "description": "The eTag of this response for caching purposes."
    },
    "kind": {
     "type": "string",
     "description": "The kind of resource this is, in this case dfareporting#userProfile.",
     "default": "dfareporting#userProfile"
    },
    "profileId": {
     "type": "string",
     "description": "The unique ID of the user profile.",
     "format": "int64"
    },
    "subAccountId": {
     "type": "string",
     "description": "The sub account ID this profile belongs to if applicable.",
     "format": "int64"
    },
    "subAccountName": {
     "type": "string",
     "description": "The sub account name this profile belongs to if applicable."
    },
    "userName": {
     "type": "string",
     "description": "The user name."
    }
   }
  },
  "UserProfileList": {
   "id": "UserProfileList",
   "type": "object",
   "description": "Represents the list of user profiles.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of this response for caching purposes."
    },
    "items": {
     "type": "array",
     "description": "The user profiles returned in this response.",
     "items": {
      "$ref": "UserProfile"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of list this is, in this case dfareporting#userProfileList.",
     "default": "dfareporting#userProfileList"
    }
   }
  }
 },
 "resources": {
  "dimensionValues": {
   "methods": {
    "query": {
     "id": "dfareporting.dimensionValues.query",
     "path": "userprofiles/{profileId}/dimensionvalues/query",
     "httpMethod": "POST",
     "description": "Retrieves list of report dimension values for a list of filters.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return.",
       "format": "int32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "The value of the nextToken from the previous result page.",
       "location": "query"
      },
      "profileId": {
       "type": "string",
       "description": "The DFA user profile ID.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "profileId"
     ],
     "request": {
      "$ref": "DimensionValueRequest"
     },
     "response": {
      "$ref": "DimensionValueList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    }
   }
  },
  "files": {
   "methods": {
    "list": {
     "id": "dfareporting.files.list",
     "path": "userprofiles/{profileId}/files",
     "httpMethod": "GET",
     "description": "Lists files for a user profile.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return.",
       "format": "int32",
       "minimum": "0",
       "maximum": "10",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "The value of the nextToken from the previous result page.",
       "location": "query"
      },
      "profileId": {
       "type": "string",
       "description": "The DFA profile ID.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "sortField": {
       "type": "string",
       "description": "The field by which to sort the list.",
       "default": "LAST_MODIFIED_TIME",
       "enum": [
        "ID",
        "LAST_MODIFIED_TIME"
       ],
       "enumDescriptions": [
        "Sort by file ID.",
        "Sort by 'lastmodifiedAt' field."
       ],
       "location": "query"
      },
      "sortOrder": {
       "type": "string",
       "description": "Order of sorted results, default is 'DESCENDING'.",
       "default": "DESCENDING",
       "enum": [
        "ASCENDING",
        "DESCENDING"
       ],
       "enumDescriptions": [
        "Ascending order.",
        "Descending order."
       ],
       "location": "query"
      }
     },
     "parameterOrder": [
      "profileId"
     ],
     "response": {
      "$ref": "FileList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    }
   }
  },
  "reports": {
   "methods": {
    "delete": {
     "id": "dfareporting.reports.delete",
     "path": "userprofiles/{profileId}/reports/{reportId}",
     "httpMethod": "DELETE",
     "description": "Deletes a report by its ID.",
     "parameters": {
      "profileId": {
       "type": "string",
       "description": "The DFA user profile ID.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "reportId": {
       "type": "string",
       "description": "The ID of the report.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "profileId",
      "reportId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    },
    "get": {
     "id": "dfareporting.reports.get",
     "path": "userprofiles/{profileId}/reports/{reportId}",
     "httpMethod": "GET",
     "description": "Retrieves a report by its ID.",
     "parameters": {
      "profileId": {
       "type": "string",
       "description": "The DFA user profile ID.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "reportId": {
       "type": "string",
       "description": "The ID of the report.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "profileId",
      "reportId"
     ],
     "response": {
      "$ref": "Report"
     },
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    },
    "insert": {
     "id": "dfareporting.reports.insert",
     "path": "userprofiles/{profileId}/reports",
     "httpMethod": "POST",
     "description": "Creates a report.",
     "parameters": {
      "profileId": {
       "type": "string",
       "description": "The DFA user profile ID.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "profileId"
     ],
     "request": {
      "$ref": "Report"
     },
     "response": {
      "$ref": "Report"
     },
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    },
    "list": {
     "id": "dfareporting.reports.list",
     "path": "userprofiles/{profileId}/reports",
     "httpMethod": "GET",
     "description": "Retrieves list of reports.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return.",
       "format": "int32",
       "minimum": "0",
       "maximum": "10",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "The value of the nextToken from the previous result page.",
       "location": "query"
      },
      "profileId": {
       "type": "string",
       "description": "The DFA user profile ID.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "sortField": {
       "type": "string",
       "description": "The field by which to sort the list.",
       "default": "LAST_MODIFIED_TIME",
       "enum": [
        "ID",
        "LAST_MODIFIED_TIME",
        "NAME"
       ],
       "enumDescriptions": [
        "Sort by report ID.",
        "Sort by 'lastModifiedTime' field.",
        "Sort by name of reports."
       ],
       "location": "query"
      },
      "sortOrder": {
       "type": "string",
       "description": "Order of sorted results, default is 'DESCENDING'.",
       "default": "DESCENDING",
       "enum": [
        "ASCENDING",
        "DESCENDING"
       ],
       "enumDescriptions": [
        "Ascending order.",
        "Descending order."
       ],
       "location": "query"
      }
     },
     "parameterOrder": [
      "profileId"
     ],
     "response": {
      "$ref": "ReportList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    },
    "patch": {
     "id": "dfareporting.reports.patch",
     "path": "userprofiles/{profileId}/reports/{reportId}",
     "httpMethod": "PATCH",
     "description": "Updates a report. This method supports patch semantics.",
     "parameters": {
      "profileId": {
       "type": "string",
       "description": "The DFA user profile ID.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "reportId": {
       "type": "string",
       "description": "The ID of the report.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "profileId",
      "reportId"
     ],
     "request": {
      "$ref": "Report"
     },
     "response": {
      "$ref": "Report"
     },
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    },
    "run": {
     "id": "dfareporting.reports.run",
     "path": "userprofiles/{profileId}/reports/{reportId}/run",
     "httpMethod": "POST",
     "description": "Runs a report.",
     "parameters": {
      "profileId": {
       "type": "string",
       "description": "The DFA profile ID.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "reportId": {
       "type": "string",
       "description": "The ID of the report.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "synchronous": {
       "type": "boolean",
       "description": "If set and true, tries to run the report synchronously.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "profileId",
      "reportId"
     ],
     "response": {
      "$ref": "File"
     },
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    },
    "update": {
     "id": "dfareporting.reports.update",
     "path": "userprofiles/{profileId}/reports/{reportId}",
     "httpMethod": "PUT",
     "description": "Updates a report.",
     "parameters": {
      "profileId": {
       "type": "string",
       "description": "The DFA user profile ID.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "reportId": {
       "type": "string",
       "description": "The ID of the report.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "profileId",
      "reportId"
     ],
     "request": {
      "$ref": "Report"
     },
     "response": {
      "$ref": "Report"
     },
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    }
   },
   "resources": {
    "files": {
     "methods": {
      "get": {
       "id": "dfareporting.reports.files.get",
       "path": "userprofiles/{profileId}/reports/{reportId}/files/{fileId}",
       "httpMethod": "GET",
       "description": "Retrieves a report file.",
       "parameters": {
        "fileId": {
         "type": "string",
         "description": "The ID of the report file.",
         "required": true,
         "format": "int64",
         "location": "path"
        },
        "profileId": {
         "type": "string",
         "description": "The DFA profile ID.",
         "required": true,
         "format": "int64",
         "location": "path"
        },
        "reportId": {
         "type": "string",
         "description": "The ID of the report.",
         "required": true,
         "format": "int64",
         "location": "path"
        }
       },
       "parameterOrder": [
        "profileId",
        "reportId",
        "fileId"
       ],
       "response": {
        "$ref": "File"
       },
       "scopes": [
        "https://www.googleapis.com/auth/dfareporting"
       ]
      },
      "list": {
       "id": "dfareporting.reports.files.list",
       "path": "userprofiles/{profileId}/reports/{reportId}/files",
       "httpMethod": "GET",
       "description": "Lists files for a report.",
       "parameters": {
        "maxResults": {
         "type": "integer",
         "description": "Maximum number of results to return.",
         "format": "int32",
         "minimum": "0",
         "maximum": "10",
         "location": "query"
        },
        "pageToken": {
         "type": "string",
         "description": "The value of the nextToken from the previous result page.",
         "location": "query"
        },
        "profileId": {
         "type": "string",
         "description": "The DFA profile ID.",
         "required": true,
         "format": "int64",
         "location": "path"
        },
        "reportId": {
         "type": "string",
         "description": "The ID of the parent report.",
         "required": true,
         "format": "int64",
         "location": "path"
        },
        "sortField": {
         "type": "string",
         "description": "The field by which to sort the list.",
         "default": "LAST_MODIFIED_TIME",
         "enum": [
          "ID",
          "LAST_MODIFIED_TIME"
         ],
         "enumDescriptions": [
          "Sort by file ID.",
          "Sort by 'lastmodifiedAt' field."
         ],
         "location": "query"
        },
        "sortOrder": {
         "type": "string",
         "description": "Order of sorted results, default is 'DESCENDING'.",
         "default": "DESCENDING",
         "enum": [
          "ASCENDING",
          "DESCENDING"
         ],
         "enumDescriptions": [
          "Ascending order.",
          "Descending order."
         ],
         "location": "query"
        }
       },
       "parameterOrder": [
        "profileId",
        "reportId"
       ],
       "response": {
        "$ref": "FileList"
       },
       "scopes": [
        "https://www.googleapis.com/auth/dfareporting"
       ]
      }
     }
    }
   }
  },
  "userProfiles": {
   "methods": {
    "get": {
     "id": "dfareporting.userProfiles.get",
     "path": "userprofiles/{profileId}",
     "httpMethod": "GET",
     "description": "Gets one user profile by ID.",
     "parameters": {
      "profileId": {
       "type": "string",
       "description": "The user profile ID.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "profileId"
     ],
     "response": {
      "$ref": "UserProfile"
     },
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    },
    "list": {
     "id": "dfareporting.userProfiles.list",
     "path": "userprofiles",
     "httpMethod": "GET",
     "description": "Retrieves list of user profiles for a user.",
     "response": {
      "$ref": "UserProfileList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/dfareporting"
     ]
    }
   }
  }
 }
}
