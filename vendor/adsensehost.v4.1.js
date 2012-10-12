{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "adsensehost:v4.1",
 "name": "adsensehost",
 "canonicalName": "AdSense Host",
 "version": "v4.1",
 "revision": "20121007",
 "title": "AdSense Host API",
 "description": "Gives AdSense Hosts access to report generation, ad code generation, and publisher management capabilities.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/adsense-16.png",
  "x32": "http://www.google.com/images/icons/product/adsense-32.png"
 },
 "documentationLink": "https://developers.google.com/adsense/host/",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/adsensehost/v4.1/",
 "basePath": "/adsensehost/v4.1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "adsensehost/v4.1/",
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
    "https://www.googleapis.com/auth/adsensehost": {
     "description": "View and manage your AdSense host data and associated accounts"
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
     "description": "Kind of resource this is, in this case adsensehost#account.",
     "default": "adsensehost#account"
    },
    "name": {
     "type": "string",
     "description": "Name of this account."
    },
    "status": {
     "type": "string",
     "description": "Approval status of this account. One of: PENDING, APPROVED, DISABLED."
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
     "description": "Kind of list this is, in this case adsensehost#accounts.",
     "default": "adsensehost#accounts"
    }
   }
  },
  "AdClient": {
   "id": "AdClient",
   "type": "object",
   "properties": {
    "arcOptIn": {
     "type": "boolean",
     "description": "Whether this ad client is opted in to ARC."
    },
    "id": {
     "type": "string",
     "description": "Unique identifier of this ad client."
    },
    "kind": {
     "type": "string",
     "description": "Kind of resource this is, in this case adsensehost#adClient.",
     "default": "adsensehost#adClient"
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
     "description": "Kind of list this is, in this case adsensehost#adClients.",
     "default": "adsensehost#adClients"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through ad clients. To retrieve the next page of results, set the next request's \"pageToken\" value to this."
    }
   }
  },
  "AdCode": {
   "id": "AdCode",
   "type": "object",
   "properties": {
    "adCode": {
     "type": "string",
     "description": "The ad code snippet."
    },
    "kind": {
     "type": "string",
     "description": "Kind this is, in this case adsensehost#adCode.",
     "default": "adsensehost#adCode"
    }
   }
  },
  "AdStyle": {
   "id": "AdStyle",
   "type": "object",
   "properties": {
    "colors": {
     "type": "object",
     "description": "The colors included in the style. These are represented as six hexadecimal characters, similar to HTML color codes, but without the leading hash.",
     "properties": {
      "background": {
       "type": "string",
       "description": "The color of the ad background."
      },
      "border": {
       "type": "string",
       "description": "The color of the ad border."
      },
      "text": {
       "type": "string",
       "description": "The color of the ad text."
      },
      "title": {
       "type": "string",
       "description": "The color of the ad title."
      },
      "url": {
       "type": "string",
       "description": "The color of the ad url."
      }
     }
    },
    "corners": {
     "type": "string",
     "description": "The style of the corners in the ad."
    },
    "font": {
     "type": "object",
     "description": "The font which is included in the style.",
     "properties": {
      "family": {
       "type": "string",
       "description": "The family of the font."
      },
      "size": {
       "type": "string",
       "description": "The size of the font."
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "Kind this is, in this case adsensehost#adStyle.",
     "default": "adsensehost#adStyle"
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
    "contentAdsSettings": {
     "type": "object",
     "description": "Settings specific to content ads (AFC) and highend mobile content ads (AFMC).",
     "properties": {
      "backupOption": {
       "type": "object",
       "description": "The backup option to be used in instances where no ad is available.",
       "properties": {
        "color": {
         "type": "string",
         "description": "Color to use when type is set to COLOR."
        },
        "type": {
         "type": "string",
         "description": "Type of the backup option. Possible values are BLANK, COLOR and URL."
        },
        "url": {
         "type": "string",
         "description": "URL to use when type is set to URL."
        }
       }
      },
      "size": {
       "type": "string",
       "description": "Size of this ad unit."
      },
      "type": {
       "type": "string",
       "description": "Type of this ad unit."
      }
     }
    },
    "customStyle": {
     "$ref": "AdStyle",
     "description": "Custom style information specific to this ad unit."
    },
    "id": {
     "type": "string",
     "description": "Unique identifier of this ad unit. This should be considered an opaque identifier; it is not safe to rely on it being in any particular format."
    },
    "kind": {
     "type": "string",
     "description": "Kind of resource this is, in this case adsensehost#adUnit.",
     "default": "adsensehost#adUnit"
    },
    "mobileContentAdsSettings": {
     "type": "object",
     "description": "Settings specific to WAP mobile content ads (AFMC).",
     "properties": {
      "markupLanguage": {
       "type": "string",
       "description": "The markup language to use for this ad unit."
      },
      "scriptingLanguage": {
       "type": "string",
       "description": "The scripting language to use for this ad unit."
      },
      "size": {
       "type": "string",
       "description": "Size of this ad unit."
      },
      "type": {
       "type": "string",
       "description": "Type of this ad unit."
      }
     }
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
     "description": "Kind of list this is, in this case adsensehost#adUnits.",
     "default": "adsensehost#adUnits"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through ad units. To retrieve the next page of results, set the next request's \"pageToken\" value to this."
    }
   }
  },
  "AssociationSession": {
   "id": "AssociationSession",
   "type": "object",
   "properties": {
    "accountId": {
     "type": "string",
     "description": "Hosted account id of the associated publisher after association. Present if status is ACCEPTED."
    },
    "id": {
     "type": "string",
     "description": "Unique identifier of this association session."
    },
    "kind": {
     "type": "string",
     "description": "Kind of resource this is, in this case adsensehost#associationSession.",
     "default": "adsensehost#associationSession"
    },
    "productCodes": {
     "type": "array",
     "description": "The products to associate with the user. Options: AFC, AFF, AFS, AFMC",
     "items": {
      "type": "string"
     }
    },
    "redirectUrl": {
     "type": "string",
     "description": "Redirect URL of this association session. Used to redirect users into the AdSense association flow."
    },
    "status": {
     "type": "string",
     "description": "Status of the completed association, available once the association callback token has been verified. One of ACCEPTED, REJECTED, or ERROR."
    },
    "userLocale": {
     "type": "string",
     "description": "The preferred locale of the user themselves when going through the AdSense association flow."
    },
    "websiteLocale": {
     "type": "string",
     "description": "The locale of the user's hosted website."
    },
    "websiteUrl": {
     "type": "string",
     "description": "The URL of the user's hosted website."
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
     "description": "Kind of resource this is, in this case adsensehost#customChannel.",
     "default": "adsensehost#customChannel"
    },
    "name": {
     "type": "string",
     "description": "Name of this custom channel."
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
     "description": "Kind of list this is, in this case adsensehost#customChannels.",
     "default": "adsensehost#customChannels"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through custom channels. To retrieve the next page of results, set the next request's \"pageToken\" value to this."
    }
   }
  },
  "Report": {
   "id": "Report",
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
     "description": "Kind this is, in this case adsensehost#report.",
     "default": "adsensehost#report"
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
     "description": "Kind of resource this is, in this case adsensehost#urlChannel.",
     "default": "adsensehost#urlChannel"
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
     "description": "Kind of list this is, in this case adsensehost#urlChannels.",
     "default": "adsensehost#urlChannels"
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
     "id": "adsensehost.accounts.get",
     "path": "accounts/{accountId}",
     "httpMethod": "GET",
     "description": "Get information about the selected associated AdSense account.",
     "parameters": {
      "accountId": {
       "type": "string",
       "description": "Account to get information about.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "accountId"
     ],
     "response": {
      "$ref": "Account"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    },
    "list": {
     "id": "adsensehost.accounts.list",
     "path": "accounts",
     "httpMethod": "GET",
     "description": "List hosted accounts associated with this AdSense account by ad client id.",
     "parameters": {
      "filterAdClientId": {
       "type": "string",
       "description": "Ad clients to list accounts for.",
       "required": true,
       "repeated": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "filterAdClientId"
     ],
     "response": {
      "$ref": "Accounts"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    }
   },
   "resources": {
    "adclients": {
     "methods": {
      "get": {
       "id": "adsensehost.accounts.adclients.get",
       "path": "accounts/{accountId}/adclients/{adClientId}",
       "httpMethod": "GET",
       "description": "Get information about one of the ad clients in the specified publisher's AdSense account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account which contains the ad client.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client to get.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "accountId",
        "adClientId"
       ],
       "response": {
        "$ref": "AdClient"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsensehost"
       ]
      },
      "list": {
       "id": "adsensehost.accounts.adclients.list",
       "path": "accounts/{accountId}/adclients",
       "httpMethod": "GET",
       "description": "List all hosted ad clients in the specified hosted account.",
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
         "format": "uint32",
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
        "https://www.googleapis.com/auth/adsensehost"
       ]
      }
     }
    },
    "adunits": {
     "methods": {
      "delete": {
       "id": "adsensehost.accounts.adunits.delete",
       "path": "accounts/{accountId}/adclients/{adClientId}/adunits/{adUnitId}",
       "httpMethod": "DELETE",
       "description": "Delete the specified ad unit from the specified publisher AdSense account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account which contains the ad unit.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client for which to get ad unit.",
         "required": true,
         "location": "path"
        },
        "adUnitId": {
         "type": "string",
         "description": "Ad unit to delete.",
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
        "https://www.googleapis.com/auth/adsensehost"
       ]
      },
      "get": {
       "id": "adsensehost.accounts.adunits.get",
       "path": "accounts/{accountId}/adclients/{adClientId}/adunits/{adUnitId}",
       "httpMethod": "GET",
       "description": "Get the specified host ad unit in this AdSense account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account which contains the ad unit.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client for which to get ad unit.",
         "required": true,
         "location": "path"
        },
        "adUnitId": {
         "type": "string",
         "description": "Ad unit to get.",
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
        "https://www.googleapis.com/auth/adsensehost"
       ]
      },
      "getAdCode": {
       "id": "adsensehost.accounts.adunits.getAdCode",
       "path": "accounts/{accountId}/adclients/{adClientId}/adunits/{adUnitId}/adcode",
       "httpMethod": "GET",
       "description": "Get ad code for the specified ad unit, attaching the specified host custom channels.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account which contains the ad client.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client with contains the ad unit.",
         "required": true,
         "location": "path"
        },
        "adUnitId": {
         "type": "string",
         "description": "Ad unit to get the code for.",
         "required": true,
         "location": "path"
        },
        "hostCustomChannelId": {
         "type": "string",
         "description": "Host custom channel to attach to the ad code.",
         "repeated": true,
         "location": "query"
        }
       },
       "parameterOrder": [
        "accountId",
        "adClientId",
        "adUnitId"
       ],
       "response": {
        "$ref": "AdCode"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsensehost"
       ]
      },
      "insert": {
       "id": "adsensehost.accounts.adunits.insert",
       "path": "accounts/{accountId}/adclients/{adClientId}/adunits",
       "httpMethod": "POST",
       "description": "Insert the supplied ad unit into the specified publisher AdSense account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account which will contain the ad unit.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client into which to insert the ad unit.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "accountId",
        "adClientId"
       ],
       "request": {
        "$ref": "AdUnit"
       },
       "response": {
        "$ref": "AdUnit"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsensehost"
       ]
      },
      "list": {
       "id": "adsensehost.accounts.adunits.list",
       "path": "accounts/{accountId}/adclients/{adClientId}/adunits",
       "httpMethod": "GET",
       "description": "List all ad units in the specified publisher's AdSense account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account which contains the ad client.",
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
         "format": "uint32",
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
        "https://www.googleapis.com/auth/adsensehost"
       ]
      },
      "patch": {
       "id": "adsensehost.accounts.adunits.patch",
       "path": "accounts/{accountId}/adclients/{adClientId}/adunits",
       "httpMethod": "PATCH",
       "description": "Update the supplied ad unit in the specified publisher AdSense account. This method supports patch semantics.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account which contains the ad client.",
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
         "description": "Ad unit to get.",
         "required": true,
         "location": "query"
        }
       },
       "parameterOrder": [
        "accountId",
        "adClientId",
        "adUnitId"
       ],
       "request": {
        "$ref": "AdUnit"
       },
       "response": {
        "$ref": "AdUnit"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsensehost"
       ]
      },
      "update": {
       "id": "adsensehost.accounts.adunits.update",
       "path": "accounts/{accountId}/adclients/{adClientId}/adunits",
       "httpMethod": "PUT",
       "description": "Update the supplied ad unit in the specified publisher AdSense account.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Account which contains the ad client.",
         "required": true,
         "location": "path"
        },
        "adClientId": {
         "type": "string",
         "description": "Ad client which contains the ad unit.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "accountId",
        "adClientId"
       ],
       "request": {
        "$ref": "AdUnit"
       },
       "response": {
        "$ref": "AdUnit"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsensehost"
       ]
      }
     }
    },
    "reports": {
     "methods": {
      "generate": {
       "id": "adsensehost.accounts.reports.generate",
       "path": "accounts/{accountId}/reports",
       "httpMethod": "GET",
       "description": "Generate an AdSense report based on the report request sent in the query parameters. Returns the result as JSON; to retrieve output in CSV format specify \"alt=csv\" as a query parameter.",
       "parameters": {
        "accountId": {
         "type": "string",
         "description": "Hosted account upon which to report.",
         "required": true,
         "location": "path"
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
         "format": "uint32",
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
         "format": "uint32",
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
        "$ref": "Report"
       },
       "scopes": [
        "https://www.googleapis.com/auth/adsensehost"
       ]
      }
     }
    }
   }
  },
  "adclients": {
   "methods": {
    "get": {
     "id": "adsensehost.adclients.get",
     "path": "adclients/{adClientId}",
     "httpMethod": "GET",
     "description": "Get information about one of the ad clients in the Host AdSense account.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client to get.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "adClientId"
     ],
     "response": {
      "$ref": "AdClient"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    },
    "list": {
     "id": "adsensehost.adclients.list",
     "path": "adclients",
     "httpMethod": "GET",
     "description": "List all host ad clients in this AdSense account.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of ad clients to include in the response, used for paging.",
       "format": "uint32",
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
      "https://www.googleapis.com/auth/adsensehost"
     ]
    }
   }
  },
  "associationsessions": {
   "methods": {
    "start": {
     "id": "adsensehost.associationsessions.start",
     "path": "associationsessions/start",
     "httpMethod": "GET",
     "description": "Create an association session for initiating an association with an AdSense user.",
     "parameters": {
      "productCode": {
       "type": "string",
       "description": "Products to associate with the user.",
       "required": true,
       "enum": [
        "AFC",
        "AFMC",
        "AFS"
       ],
       "enumDescriptions": [
        "AdSense For Content",
        "AdSense For Mobile Content",
        "AdSense For Search"
       ],
       "repeated": true,
       "location": "query"
      },
      "userLocale": {
       "type": "string",
       "description": "The preferred locale of the user.",
       "location": "query"
      },
      "websiteLocale": {
       "type": "string",
       "description": "The locale of the user's hosted website.",
       "location": "query"
      },
      "websiteUrl": {
       "type": "string",
       "description": "The URL of the user's hosted website.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "productCode",
      "websiteUrl"
     ],
     "response": {
      "$ref": "AssociationSession"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    },
    "verify": {
     "id": "adsensehost.associationsessions.verify",
     "path": "associationsessions/verify",
     "httpMethod": "GET",
     "description": "Verify an association session after the association callback returns from AdSense signup.",
     "parameters": {
      "token": {
       "type": "string",
       "description": "The token returned to the association callback URL.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "token"
     ],
     "response": {
      "$ref": "AssociationSession"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    }
   }
  },
  "customchannels": {
   "methods": {
    "delete": {
     "id": "adsensehost.customchannels.delete",
     "path": "adclients/{adClientId}/customchannels/{customChannelId}",
     "httpMethod": "DELETE",
     "description": "Delete a specific custom channel from the host AdSense account.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client from which to delete the custom channel.",
       "required": true,
       "location": "path"
      },
      "customChannelId": {
       "type": "string",
       "description": "Custom channel to delete.",
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
      "https://www.googleapis.com/auth/adsensehost"
     ]
    },
    "get": {
     "id": "adsensehost.customchannels.get",
     "path": "adclients/{adClientId}/customchannels/{customChannelId}",
     "httpMethod": "GET",
     "description": "Get a specific custom channel from the host AdSense account.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client from which to get the custom channel.",
       "required": true,
       "location": "path"
      },
      "customChannelId": {
       "type": "string",
       "description": "Custom channel to get.",
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
      "https://www.googleapis.com/auth/adsensehost"
     ]
    },
    "insert": {
     "id": "adsensehost.customchannels.insert",
     "path": "adclients/{adClientId}/customchannels",
     "httpMethod": "POST",
     "description": "Add a new custom channel to the host AdSense account.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client to which the new custom channel will be added.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "adClientId"
     ],
     "request": {
      "$ref": "CustomChannel"
     },
     "response": {
      "$ref": "CustomChannel"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    },
    "list": {
     "id": "adsensehost.customchannels.list",
     "path": "adclients/{adClientId}/customchannels",
     "httpMethod": "GET",
     "description": "List all host custom channels in this AdSense account.",
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
       "format": "uint32",
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
      "https://www.googleapis.com/auth/adsensehost"
     ]
    },
    "patch": {
     "id": "adsensehost.customchannels.patch",
     "path": "adclients/{adClientId}/customchannels",
     "httpMethod": "PATCH",
     "description": "Update a custom channel in the host AdSense account. This method supports patch semantics.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client in which the custom channel will be updated.",
       "required": true,
       "location": "path"
      },
      "customChannelId": {
       "type": "string",
       "description": "Custom channel to get.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "adClientId",
      "customChannelId"
     ],
     "request": {
      "$ref": "CustomChannel"
     },
     "response": {
      "$ref": "CustomChannel"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    },
    "update": {
     "id": "adsensehost.customchannels.update",
     "path": "adclients/{adClientId}/customchannels",
     "httpMethod": "PUT",
     "description": "Update a custom channel in the host AdSense account.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client in which the custom channel will be updated.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "adClientId"
     ],
     "request": {
      "$ref": "CustomChannel"
     },
     "response": {
      "$ref": "CustomChannel"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    }
   }
  },
  "reports": {
   "methods": {
    "generate": {
     "id": "adsensehost.reports.generate",
     "path": "reports",
     "httpMethod": "GET",
     "description": "Generate an AdSense report based on the report request sent in the query parameters. Returns the result as JSON; to retrieve output in CSV format specify \"alt=csv\" as a query parameter.",
     "parameters": {
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
       "format": "uint32",
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
       "format": "uint32",
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
      "$ref": "Report"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    }
   }
  },
  "urlchannels": {
   "methods": {
    "delete": {
     "id": "adsensehost.urlchannels.delete",
     "path": "adclients/{adClientId}/urlchannels/{urlChannelId}",
     "httpMethod": "DELETE",
     "description": "Delete a URL channel from the host AdSense account.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client from which to delete the URL channel.",
       "required": true,
       "location": "path"
      },
      "urlChannelId": {
       "type": "string",
       "description": "URL channel to delete.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "adClientId",
      "urlChannelId"
     ],
     "response": {
      "$ref": "UrlChannel"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    },
    "insert": {
     "id": "adsensehost.urlchannels.insert",
     "path": "adclients/{adClientId}/urlchannels",
     "httpMethod": "POST",
     "description": "Add a new URL channel to the host AdSense account.",
     "parameters": {
      "adClientId": {
       "type": "string",
       "description": "Ad client to which the new URL channel will be added.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "adClientId"
     ],
     "request": {
      "$ref": "UrlChannel"
     },
     "response": {
      "$ref": "UrlChannel"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adsensehost"
     ]
    },
    "list": {
     "id": "adsensehost.urlchannels.list",
     "path": "adclients/{adClientId}/urlchannels",
     "httpMethod": "GET",
     "description": "List all host URL channels in the host AdSense account.",
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
       "format": "uint32",
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
      "https://www.googleapis.com/auth/adsensehost"
     ]
    }
   }
  }
 }
}
