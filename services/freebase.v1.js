{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "freebase:v1",
 "name": "freebase",
 "version": "v1",
 "revision": "20120807",
 "title": "Freebase API",
 "description": "Lets you access the Freebase repository of open data.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/freebase-16.png",
  "x32": "http://www.google.com/images/icons/product/freebase-32.png"
 },
 "documentationLink": "http://wiki.freebase.com/wiki/API",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/freebase/v1/",
 "basePath": "/freebase/v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "freebase/v1/",
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
    "https://www.googleapis.com/auth/freebase": {
     "description": "Sign in to Freebase with your account"
    }
   }
  }
 },
 "schemas": {
  "ContentserviceGet": {
   "id": "ContentserviceGet",
   "type": "object",
   "properties": {
    "result": {
     "type": "string",
     "description": "The text requested."
    }
   }
  },
  "TopicLookup": {
   "id": "TopicLookup",
   "type": "object",
   "properties": {
    "id": {
     "type": "string"
    },
    "property": {
     "type": "object",
     "properties": {
      "/freebase/object_profile/linkcount": {
       "$ref": "TopicStatslinkcount"
      }
     },
     "additionalProperties": {
      "$ref": "TopicPropertyvalue"
     }
    }
   }
  },
  "TopicPropertyvalue": {
   "id": "TopicPropertyvalue",
   "type": "object",
   "properties": {
    "values": {
     "type": "array",
     "items": {
      "$ref": "TopicValue"
     }
    },
    "valuetype": {
     "type": "string"
    }
   }
  },
  "TopicStatslinkcount": {
   "id": "TopicStatslinkcount",
   "type": "object",
   "properties": {
    "type": {
     "type": "string",
     "default": "custom"
    },
    "values": {
     "type": "array",
     "items": {
      "type": "object",
      "properties": {
       "count": {
        "type": "integer",
        "format": "int32"
       },
       "id": {
        "type": "string"
       },
       "values": {
        "type": "array",
        "items": {
         "type": "object",
         "properties": {
          "count": {
           "type": "integer",
           "format": "int32"
          },
          "id": {
           "type": "string"
          },
          "values": {
           "type": "array",
           "items": {
            "type": "object",
            "properties": {
             "count": {
              "type": "integer",
              "format": "int32"
             },
             "id": {
              "type": "string"
             }
            }
           }
          }
         }
        }
       }
      }
     }
    }
   }
  },
  "TopicValue": {
   "id": "TopicValue",
   "type": "object",
   "properties": {
    "creator": {
     "type": "string"
    },
    "id": {
     "type": "string"
    },
    "lang": {
     "type": "string"
    },
    "property": {
     "type": "object",
     "additionalProperties": {
      "$ref": "TopicPropertyvalue"
     }
    },
    "text": {
     "type": "string"
    },
    "value": {
     "type": "any"
    }
   }
  }
 },
 "methods": {
  "image": {
   "id": "freebase.image",
   "path": "image{/id*}",
   "httpMethod": "GET",
   "description": "Returns the scaled/cropped image attached to a freebase node.",
   "parameters": {
    "fallbackid": {
     "type": "string",
     "description": "Use the image associated with this secondary id if no image is associated with the primary id.",
     "default": "/freebase/no_image_png",
     "pattern": "/[^.]*$",
     "location": "query"
    },
    "id": {
     "type": "string",
     "description": "Freebase entity or content id, mid, or guid.",
     "required": true,
     "repeated": true,
     "location": "path"
    },
    "maxheight": {
     "type": "integer",
     "description": "Maximum height in pixels for resulting image.",
     "format": "uint32",
     "maximum": "4096",
     "location": "query"
    },
    "maxwidth": {
     "type": "integer",
     "description": "Maximum width in pixels for resulting image.",
     "format": "uint32",
     "maximum": "4096",
     "location": "query"
    },
    "mode": {
     "type": "string",
     "description": "Method used to scale or crop image.",
     "default": "fit",
     "enum": [
      "fill",
      "fillcrop",
      "fillcropmid",
      "fit"
     ],
     "enumDescriptions": [
      "Fill rectangle completely with image, relax constraint on one dimension if necessary.",
      "Fill rectangle with image, crop image to maintain rectangle dimensions.",
      "Fill rectangle with image, center horizontally, crop left and right.",
      "Fit image inside rectangle, leave empty space in one dimension if necessary."
     ],
     "location": "query"
    },
    "pad": {
     "type": "boolean",
     "description": "A boolean specifying whether the resulting image should be padded up to the requested dimensions.",
     "default": "false",
     "location": "query"
    }
   },
   "parameterOrder": [
    "id"
   ],
   "supportsMediaDownload": true
  },
  "mqlread": {
   "id": "freebase.mqlread",
   "path": "mqlread",
   "httpMethod": "GET",
   "description": "Performs MQL Queries.",
   "parameters": {
    "as_of_time": {
     "type": "string",
     "description": "Run the query as it would've been run at the specified point in time.",
     "location": "query"
    },
    "callback": {
     "type": "string",
     "description": "JS method name for JSONP callbacks.",
     "pattern": "([A-Za-z0-9_$.]|\\[|\\])+",
     "location": "query"
    },
    "cost": {
     "type": "boolean",
     "description": "Show the costs or not.",
     "default": "false",
     "location": "query"
    },
    "cursor": {
     "type": "string",
     "description": "The mql cursor.",
     "location": "query"
    },
    "dateline": {
     "type": "string",
     "description": "The dateline that you get in a mqlwrite response to ensure consistent results.",
     "location": "query"
    },
    "html_escape": {
     "type": "boolean",
     "description": "Whether or not to escape entities.",
     "default": "true",
     "location": "query"
    },
    "indent": {
     "type": "integer",
     "description": "How many spaces to indent the json.",
     "default": "0",
     "format": "uint32",
     "maximum": "10",
     "location": "query"
    },
    "lang": {
     "type": "string",
     "description": "The language of the results - an id of a /type/lang object.",
     "default": "/lang/en",
     "location": "query"
    },
    "query": {
     "type": "string",
     "description": "An envelope containing a single MQL query.",
     "required": true,
     "location": "query"
    },
    "uniqueness_failure": {
     "type": "string",
     "description": "How MQL responds to uniqueness failures.",
     "default": "hard",
     "enum": [
      "hard",
      "soft"
     ],
     "enumDescriptions": [
      "Be strict - throw an error.",
      "Just return the first encountered object."
     ],
     "location": "query"
    }
   },
   "parameterOrder": [
    "query"
   ],
   "supportsMediaDownload": true
  },
  "mqlwrite": {
   "id": "freebase.mqlwrite",
   "path": "mqlwrite",
   "httpMethod": "GET",
   "description": "Performs MQL Write Operations.",
   "parameters": {
    "callback": {
     "type": "string",
     "description": "JS method name for JSONP callbacks.",
     "pattern": "([A-Za-z0-9_$.]|\\[|\\])+",
     "location": "query"
    },
    "dateline": {
     "type": "string",
     "description": "The dateline that you get in a mqlwrite response to ensure consistent results.",
     "location": "query"
    },
    "indent": {
     "type": "integer",
     "description": "How many spaces to indent the json.",
     "default": "0",
     "format": "uint32",
     "maximum": "10",
     "location": "query"
    },
    "query": {
     "type": "string",
     "description": "An MQL query with write directives.",
     "required": true,
     "location": "query"
    },
    "use_permission_of": {
     "type": "string",
     "description": "Use the same permission node of the object with the specified id.",
     "location": "query"
    }
   },
   "parameterOrder": [
    "query"
   ],
   "scopes": [
    "https://www.googleapis.com/auth/freebase"
   ],
   "supportsMediaDownload": true
  }
 },
 "resources": {
  "text": {
   "methods": {
    "get": {
     "id": "freebase.text.get",
     "path": "text{/id*}",
     "httpMethod": "GET",
     "description": "Returns blob attached to node at specified id as HTML",
     "parameters": {
      "format": {
       "type": "string",
       "description": "Sanitizing transformation.",
       "default": "plain",
       "enum": [
        "html",
        "plain",
        "raw"
       ],
       "enumDescriptions": [
        "Return valid, sanitized html.",
        "Return plain text - strip html tags.",
        "Return the entire content as-is."
       ],
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "The id of the item that you want data about",
       "required": true,
       "repeated": true,
       "location": "path"
      },
      "maxlength": {
       "type": "integer",
       "description": "The max number of characters to return. Valid only for 'plain' format.",
       "format": "uint32",
       "location": "query"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "response": {
      "$ref": "ContentserviceGet"
     }
    }
   }
  },
  "topic": {
   "methods": {
    "lookup": {
     "id": "freebase.topic.lookup",
     "path": "topic{/id*}",
     "httpMethod": "GET",
     "description": "Get properties and meta-data about a topic.",
     "parameters": {
      "dateline": {
       "type": "string",
       "description": "Determines how up-to-date the data returned is. A unix epoch time, a guid or a 'now'",
       "location": "query"
      },
      "filter": {
       "type": "string",
       "description": "A frebase domain, type or property id, 'suggest', 'commons', or 'all'. Filter the results and returns only appropriate properties.",
       "repeated": true,
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "The id of the item that you want data about.",
       "required": true,
       "repeated": true,
       "location": "path"
      },
      "lang": {
       "type": "string",
       "description": "The language you 'd like the content in - a freebase /type/lang language key.",
       "default": "en",
       "location": "query"
      },
      "limit": {
       "type": "integer",
       "description": "The maximum number of property values to return for each property.",
       "default": "10",
       "format": "uint32",
       "location": "query"
      },
      "raw": {
       "type": "boolean",
       "description": "Do not apply any constraints, or get any names.",
       "default": "false",
       "location": "query"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "response": {
      "$ref": "TopicLookup"
     }
    }
   }
  }
 }
}
