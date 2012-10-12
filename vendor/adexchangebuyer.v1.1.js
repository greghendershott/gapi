{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "adexchangebuyer:v1.1",
 "name": "adexchangebuyer",
 "version": "v1.1",
 "revision": "20120718",
 "title": "Ad Exchange Buyer API",
 "description": "Lets you manage your Ad Exchange Buyer account.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/doubleclick-16.gif",
  "x32": "http://www.google.com/images/icons/product/doubleclick-32.gif"
 },
 "documentationLink": "https://developers.google.com/ad-exchange/buyer-rest",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/adexchangebuyer/v1.1/",
 "basePath": "/adexchangebuyer/v1.1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "adexchangebuyer/v1.1/",
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
    "https://www.googleapis.com/auth/adexchange.buyer": {
     "description": "Manage your Ad Exchange buyer account configuration"
    }
   }
  }
 },
 "schemas": {
  "Account": {
   "id": "Account",
   "type": "object",
   "description": "Configuration data for an Ad Exchange buyer account.",
   "properties": {
    "bidderLocation": {
     "type": "array",
     "description": "Your bidder locations that have distinct URLs.",
     "items": {
      "type": "object",
      "properties": {
       "maximumQps": {
        "type": "integer",
        "description": "The maximum queries per second the Ad Exchange will send.",
        "format": "int32"
       },
       "region": {
        "type": "string",
        "description": "The geographical region the Ad Exchange should send requests from. Only used by some quota systems, but always setting the value is recommended. Allowed values:  \n- ASIA \n- EUROPE \n- US_EAST \n- US_WEST"
       },
       "url": {
        "type": "string",
        "description": "The URL to which the Ad Exchange will send bid requests."
       }
      }
     }
    },
    "cookieMatchingNid": {
     "type": "string",
     "description": "The nid parameter value used in cookie match requests. Please contact your technical account manager if you need to change this."
    },
    "cookieMatchingUrl": {
     "type": "string",
     "description": "The base URL used in cookie match requests."
    },
    "id": {
     "type": "integer",
     "description": "Account id.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "adexchangebuyer#account"
    },
    "maximumTotalQps": {
     "type": "integer",
     "description": "The sum of all bidderLocation.maximumQps values cannot exceed this. Please contact your technical account manager if you need to change this.",
     "format": "int32"
    }
   }
  },
  "AccountsList": {
   "id": "AccountsList",
   "type": "object",
   "description": "An account feed lists Ad Exchange buyer accounts that the user has access to. Each entry in the feed corresponds to a single buyer account.",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of accounts.",
     "items": {
      "$ref": "Account"
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "adexchangebuyer#accountsList"
    }
   }
  },
  "Creative": {
   "id": "Creative",
   "type": "object",
   "description": "A creative and its classification data.",
   "properties": {
    "HTMLSnippet": {
     "type": "string",
     "description": "The HTML snippet that displays the ad when inserted in the web page. If set, videoURL should not be set."
    },
    "accountId": {
     "type": "integer",
     "description": "Account id.",
     "format": "int32",
     "annotations": {
      "required": [
       "adexchangebuyer.creatives.insert"
      ]
     }
    },
    "advertiserId": {
     "type": "array",
     "description": "Detected advertiser id, if any. Read-only. This field should not be set in requests.",
     "items": {
      "type": "string",
      "format": "int64"
     }
    },
    "advertiserName": {
     "type": "string",
     "description": "The name of the company being advertised in the creative.",
     "annotations": {
      "required": [
       "adexchangebuyer.creatives.insert"
      ]
     }
    },
    "attribute": {
     "type": "array",
     "description": "All attributes for the ads that may be shown from this snippet.",
     "items": {
      "type": "integer",
      "format": "int32"
     }
    },
    "buyerCreativeId": {
     "type": "string",
     "description": "A buyer-specific id identifying the creative in this ad.",
     "annotations": {
      "required": [
       "adexchangebuyer.creatives.insert"
      ]
     }
    },
    "clickThroughUrl": {
     "type": "array",
     "description": "The set of destination urls for the snippet.",
     "items": {
      "type": "string"
     },
     "annotations": {
      "required": [
       "adexchangebuyer.creatives.insert"
      ]
     }
    },
    "disapprovalReasons": {
     "type": "array",
     "description": "The reason for disapproval, if any. Note that not all disapproval reasons may be categorized, so it is possible for the creative to have a status of DISAPPROVED with an empty list for disapproval_reasons. In this case, please reach out to your TAM to help debug the issue. Read-only. This field should not be set in requests.",
     "items": {
      "type": "string"
     }
    },
    "height": {
     "type": "integer",
     "description": "Ad height.",
     "format": "int32",
     "annotations": {
      "required": [
       "adexchangebuyer.creatives.insert"
      ]
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "adexchangebuyer#creative"
    },
    "productCategories": {
     "type": "array",
     "description": "Detected product categories, if any. Read-only. This field should not be set in requests.",
     "items": {
      "type": "integer",
      "format": "int32"
     }
    },
    "sensitiveCategories": {
     "type": "array",
     "description": "Detected sensitive categories, if any. Read-only. This field should not be set in requests.",
     "items": {
      "type": "integer",
      "format": "int32"
     }
    },
    "status": {
     "type": "string",
     "description": "Creative serving status. Read-only. This field should not be set in requests."
    },
    "vendorType": {
     "type": "array",
     "description": "All vendor types for the ads that may be shown from this snippet.",
     "items": {
      "type": "integer",
      "format": "int32"
     }
    },
    "videoURL": {
     "type": "string",
     "description": "The url to fetch a video ad. If set, HTMLSnippet should not be set."
    },
    "width": {
     "type": "integer",
     "description": "Ad width.",
     "format": "int32",
     "annotations": {
      "required": [
       "adexchangebuyer.creatives.insert"
      ]
     }
    }
   }
  },
  "CreativesList": {
   "id": "CreativesList",
   "type": "object",
   "description": "The creatives feed lists the active creatives for the Ad Exchange buyer accounts that the user has access to. Each entry in the feed corresponds to a single creative.",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of creatives.",
     "items": {
      "$ref": "Creative"
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "adexchangebuyer#creativesList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Continuation token used to page through creatives. To retrieve the next page of results, set the next request's \"pageToken\" value to this."
    }
   }
  },
  "DirectDeal": {
   "id": "DirectDeal",
   "type": "object",
   "description": "The configuration data for an Ad Exchange direct deal.",
   "properties": {
    "accountId": {
     "type": "integer",
     "description": "The account id of the buyer this deal is for.",
     "format": "int32"
    },
    "advertiser": {
     "type": "string",
     "description": "The name of the advertiser this deal is for."
    },
    "currencyCode": {
     "type": "string",
     "description": "The currency code that applies to the fixed_cpm value. If not set then assumed to be USD."
    },
    "endTime": {
     "type": "string",
     "description": "End time for when this deal stops being active. If not set then this deal is valid until manually disabled by the publisher. In seconds since the epoch.",
     "format": "int64"
    },
    "fixedCpm": {
     "type": "string",
     "description": "The fixed price for this direct deal. In cpm micros of currency according to currency_code.",
     "format": "int64"
    },
    "id": {
     "type": "string",
     "description": "Deal id.",
     "format": "int64"
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "adexchangebuyer#directDeal"
    },
    "sellerNetwork": {
     "type": "string",
     "description": "The name of the publisher offering this direct deal."
    },
    "startTime": {
     "type": "string",
     "description": "Start time for when this deal becomes active. If not set then this deal is active immediately upon creation. In seconds since the epoch.",
     "format": "int64"
    }
   }
  },
  "DirectDealsList": {
   "id": "DirectDealsList",
   "type": "object",
   "description": "A direct deals feed lists Direct Deals the Ad Exchange buyer account has access to. This includes direct deals set up for the buyer account as well as its merged stream seats.",
   "properties": {
    "directDeals": {
     "type": "array",
     "description": "A list of direct deals relevant for your account.",
     "items": {
      "$ref": "DirectDeal"
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "adexchangebuyer#directDealsList"
    }
   }
  }
 },
 "resources": {
  "accounts": {
   "methods": {
    "get": {
     "id": "adexchangebuyer.accounts.get",
     "path": "accounts/{id}",
     "httpMethod": "GET",
     "description": "Gets one account by ID.",
     "parameters": {
      "id": {
       "type": "integer",
       "description": "The account id",
       "required": true,
       "format": "int32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "response": {
      "$ref": "Account"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adexchange.buyer"
     ]
    },
    "list": {
     "id": "adexchangebuyer.accounts.list",
     "path": "accounts",
     "httpMethod": "GET",
     "description": "Retrieves the authenticated user's list of accounts.",
     "response": {
      "$ref": "AccountsList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adexchange.buyer"
     ]
    },
    "patch": {
     "id": "adexchangebuyer.accounts.patch",
     "path": "accounts/{id}",
     "httpMethod": "PATCH",
     "description": "Updates an existing account. This method supports patch semantics.",
     "parameters": {
      "id": {
       "type": "integer",
       "description": "The account id",
       "required": true,
       "format": "int32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "request": {
      "$ref": "Account"
     },
     "response": {
      "$ref": "Account"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adexchange.buyer"
     ]
    },
    "update": {
     "id": "adexchangebuyer.accounts.update",
     "path": "accounts/{id}",
     "httpMethod": "PUT",
     "description": "Updates an existing account.",
     "parameters": {
      "id": {
       "type": "integer",
       "description": "The account id",
       "required": true,
       "format": "int32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "request": {
      "$ref": "Account"
     },
     "response": {
      "$ref": "Account"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adexchange.buyer"
     ]
    }
   }
  },
  "creatives": {
   "methods": {
    "get": {
     "id": "adexchangebuyer.creatives.get",
     "path": "creatives/{accountId}/{buyerCreativeId}",
     "httpMethod": "GET",
     "description": "Gets the status for a single creative.",
     "parameters": {
      "accountId": {
       "type": "integer",
       "description": "The id for the account that will serve this creative.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "buyerCreativeId": {
       "type": "string",
       "description": "The buyer-specific id for this creative.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "accountId",
      "buyerCreativeId"
     ],
     "response": {
      "$ref": "Creative"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adexchange.buyer"
     ]
    },
    "insert": {
     "id": "adexchangebuyer.creatives.insert",
     "path": "creatives",
     "httpMethod": "POST",
     "description": "Submit a new creative.",
     "request": {
      "$ref": "Creative"
     },
     "response": {
      "$ref": "Creative"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adexchange.buyer"
     ]
    },
    "list": {
     "id": "adexchangebuyer.creatives.list",
     "path": "creatives",
     "httpMethod": "GET",
     "description": "Retrieves a list of the authenticated user's active creatives.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of entries returned on one result page. If not set, the default is 100. Optional.",
       "format": "uint32",
       "minimum": "1",
       "maximum": "1000",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token, used to page through ad clients. To retrieve the next page, set this parameter to the value of \"nextPageToken\" from the previous response. Optional.",
       "location": "query"
      },
      "statusFilter": {
       "type": "string",
       "description": "When specified, only creatives having the given status are returned.",
       "enum": [
        "approved",
        "disapproved",
        "not_checked"
       ],
       "enumDescriptions": [
        "Creatives which have been approved.",
        "Creatives which have been disapproved.",
        "Creatives whose status is not yet checked."
       ],
       "location": "query"
      }
     },
     "response": {
      "$ref": "CreativesList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adexchange.buyer"
     ]
    }
   }
  },
  "directDeals": {
   "methods": {
    "get": {
     "id": "adexchangebuyer.directDeals.get",
     "path": "directdeals/{id}",
     "httpMethod": "GET",
     "description": "Gets one direct deal by ID.",
     "parameters": {
      "id": {
       "type": "string",
       "description": "The direct deal id",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "response": {
      "$ref": "DirectDeal"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adexchange.buyer"
     ]
    },
    "list": {
     "id": "adexchangebuyer.directDeals.list",
     "path": "directdeals",
     "httpMethod": "GET",
     "description": "Retrieves the authenticated user's list of direct deals.",
     "response": {
      "$ref": "DirectDealsList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/adexchange.buyer"
     ]
    }
   }
  }
 }
}
