{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "discovery:v1",
 "name": "discovery",
 "version": "v1",
 "title": "APIs Discovery Service",
 "description": "Lets you discover information about other Google APIs, such as what APIs are available, the resource and method details for each API",
 "icons": {
  "x16": "http://www.google.com/images/icons/feature/filing_cabinet_search-g16.png",
  "x32": "http://www.google.com/images/icons/feature/filing_cabinet_search-g32.png"
 },
 "documentationLink": "https://developers.google.com/discovery/",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/discovery/v1/",
 "basePath": "/discovery/v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "discovery/v1/",
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
  "DirectoryList": {
   "id": "DirectoryList",
   "type": "object",
   "properties": {
    "discoveryVersion": {
     "type": "string",
     "description": "Indicate the version of the Discovery API used to generate this doc.",
     "default": "v1"
    },
    "items": {
     "type": "array",
     "description": "The individual directory entries. One entry per api/version pair.",
     "items": {
      "type": "object",
      "properties": {
       "description": {
        "type": "string",
        "description": "The description of this API."
       },
       "discoveryLink": {
        "type": "string",
        "description": "A link to the discovery document."
       },
       "discoveryRestUrl": {
        "type": "string",
        "description": "The url for the discovery REST document."
       },
       "documentationLink": {
        "type": "string",
        "description": "A link to human readable documentation for the API."
       },
       "icons": {
        "type": "object",
        "description": "Links to 16x16 and 32x32 icons representing the API.",
        "properties": {
         "x16": {
          "type": "string",
          "description": "The url of the 16x16 icon."
         },
         "x32": {
          "type": "string",
          "description": "The url of the 32x32 icon."
         }
        }
       },
       "id": {
        "type": "string",
        "description": "The id of this API."
       },
       "kind": {
        "type": "string",
        "description": "The kind for this response.",
        "default": "discovery#directoryItem"
       },
       "labels": {
        "type": "array",
        "description": "Labels for the status of this API, such as labs or deprecated.",
        "items": {
         "type": "string"
        }
       },
       "name": {
        "type": "string",
        "description": "The name of the API."
       },
       "preferred": {
        "type": "boolean",
        "description": "True if this version is the preferred version to use."
       },
       "title": {
        "type": "string",
        "description": "The title of this API."
       },
       "version": {
        "type": "string",
        "description": "The version of the API."
       }
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind for this response.",
     "default": "discovery#directoryList"
    }
   }
  },
  "JsonSchema": {
   "id": "JsonSchema",
   "type": "object",
   "properties": {
    "$ref": {
     "type": "string",
     "description": "A reference to another schema. The value of this property is the \"id\" of another schema."
    },
    "additionalProperties": {
     "$ref": "JsonSchema",
     "description": "If this is a schema for an object, this property is the schema for any additional properties with dynamic keys on this object."
    },
    "annotations": {
     "type": "object",
     "description": "Additional information about this property.",
     "properties": {
      "required": {
       "type": "array",
       "description": "A list of methods for which this property is required on requests.",
       "items": {
        "type": "string"
       }
      }
     }
    },
    "default": {
     "type": "string",
     "description": "The default value of this property (if one exists)."
    },
    "description": {
     "type": "string",
     "description": "A description of this object."
    },
    "enum": {
     "type": "array",
     "description": "Values this parameter may take (if it is an enum).",
     "items": {
      "type": "string"
     }
    },
    "enumDescriptions": {
     "type": "array",
     "description": "The descriptions for the enums. Each position maps to the corresponding value in the \"enum\" array.",
     "items": {
      "type": "string"
     }
    },
    "format": {
     "type": "string",
     "description": "An additional regular expression or key that helps constrain the value. For more details see: http://tools.ietf.org/html/draft-zyp-json-schema-03#section-5.23"
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for this schema."
    },
    "items": {
     "$ref": "JsonSchema",
     "description": "If this is a schema for an array, this property is the schema for each element in the array."
    },
    "location": {
     "type": "string",
     "description": "Whether this parameter goes in the query or the path for REST requests."
    },
    "maximum": {
     "type": "string",
     "description": "The maximum value of this parameter."
    },
    "minimum": {
     "type": "string",
     "description": "The minimum value of this parameter."
    },
    "pattern": {
     "type": "string",
     "description": "The regular expression this parameter must conform to. Uses Java 6 regex format: http://docs.oracle.com/javase/6/docs/api/java/util/regex/Pattern.html"
    },
    "properties": {
     "type": "object",
     "description": "If this is a schema for an object, list the schema for each property of this object.",
     "additionalProperties": {
      "$ref": "JsonSchema",
      "description": "A single property of this object. The value is itself a JSON Schema object describing this property."
     }
    },
    "repeated": {
     "type": "boolean",
     "description": "Whether this parameter may appear multiple times."
    },
    "required": {
     "type": "boolean",
     "description": "Whether the parameter is required."
    },
    "type": {
     "type": "string",
     "description": "The value type for this schema. A list of values can be found here: http://tools.ietf.org/html/draft-zyp-json-schema-03#section-5.1"
    }
   }
  },
  "RestDescription": {
   "id": "RestDescription",
   "type": "object",
   "properties": {
    "auth": {
     "type": "object",
     "description": "Authentication information.",
     "properties": {
      "oauth2": {
       "type": "object",
       "description": "OAuth 2.0 authentication information.",
       "properties": {
        "scopes": {
         "type": "object",
         "description": "Available OAuth 2.0 scopes.",
         "additionalProperties": {
          "type": "object",
          "description": "The scope value.",
          "properties": {
           "description": {
            "type": "string",
            "description": "Description of scope."
           }
          }
         }
        }
       }
      }
     }
    },
    "basePath": {
     "type": "string",
     "description": "[DEPRECATED] The base path for REST requests."
    },
    "baseUrl": {
     "type": "string",
     "description": "[DEPRECATED] The base URL for REST requests."
    },
    "batchPath": {
     "type": "string",
     "description": "The path for REST batch requests.",
     "default": "batch"
    },
    "canonicalName": {
     "type": "string",
     "description": "Indicates how the API name should be capitalized and split into various parts. Useful for generating pretty class names."
    },
    "description": {
     "type": "string",
     "description": "The description of this API."
    },
    "discoveryVersion": {
     "type": "string",
     "description": "Indicate the version of the Discovery API used to generate this doc.",
     "default": "v1"
    },
    "documentationLink": {
     "type": "string",
     "description": "A link to human readable documentation for the API."
    },
    "features": {
     "type": "array",
     "description": "A list of supported features for this API.",
     "items": {
      "type": "string"
     }
    },
    "icons": {
     "type": "object",
     "description": "Links to 16x16 and 32x32 icons representing the API.",
     "properties": {
      "x16": {
       "type": "string",
       "description": "The url of the 16x16 icon."
      },
      "x32": {
       "type": "string",
       "description": "The url of the 32x32 icon."
      }
     }
    },
    "id": {
     "type": "string",
     "description": "The id of this API."
    },
    "kind": {
     "type": "string",
     "description": "The kind for this response.",
     "default": "discovery#restDescription"
    },
    "labels": {
     "type": "array",
     "description": "Labels for the status of this API, such as labs or deprecated.",
     "items": {
      "type": "string"
     }
    },
    "methods": {
     "type": "object",
     "description": "API-level methods for this API.",
     "additionalProperties": {
      "$ref": "RestMethod",
      "description": "An individual method description."
     }
    },
    "name": {
     "type": "string",
     "description": "The name of this API."
    },
    "parameters": {
     "type": "object",
     "description": "Common parameters that apply across all apis.",
     "additionalProperties": {
      "$ref": "JsonSchema",
      "description": "Description of a single parameter."
     }
    },
    "protocol": {
     "type": "string",
     "description": "The protocol described by this document.",
     "default": "rest"
    },
    "resources": {
     "type": "object",
     "description": "The resources in this API.",
     "additionalProperties": {
      "$ref": "RestResource",
      "description": "An individual resource description. Contains methods and sub-resources related to this resource."
     }
    },
    "revision": {
     "type": "string",
     "description": "The version of this API."
    },
    "rootUrl": {
     "type": "string",
     "description": "The root url under which all API services live."
    },
    "schemas": {
     "type": "object",
     "description": "The schemas for this API.",
     "additionalProperties": {
      "$ref": "JsonSchema",
      "description": "An individual schema description."
     }
    },
    "servicePath": {
     "type": "string",
     "description": "The base path for all REST requests."
    },
    "title": {
     "type": "string",
     "description": "The title of this API."
    },
    "version": {
     "type": "string",
     "description": "The version of this API."
    }
   }
  },
  "RestMethod": {
   "id": "RestMethod",
   "type": "object",
   "properties": {
    "description": {
     "type": "string",
     "description": "Description of this method."
    },
    "httpMethod": {
     "type": "string",
     "description": "HTTP method used by this method."
    },
    "id": {
     "type": "string",
     "description": "A unique ID for this method. This property can be used to match methods between different versions of Discovery."
    },
    "mediaUpload": {
     "type": "object",
     "description": "Media upload parameters.",
     "properties": {
      "accept": {
       "type": "array",
       "description": "MIME Media Ranges for acceptable media uploads to this method.",
       "items": {
        "type": "string"
       }
      },
      "maxSize": {
       "type": "string",
       "description": "Maximum size of a media upload, such as \"1MB\", \"2GB\" or \"3TB\"."
      },
      "protocols": {
       "type": "object",
       "description": "Supported upload protocols.",
       "properties": {
        "resumable": {
         "type": "object",
         "description": "Supports the Resumable Media Upload protocol.",
         "properties": {
          "multipart": {
           "type": "boolean",
           "description": "True if this endpoint supports uploading multipart media.",
           "default": "true"
          },
          "path": {
           "type": "string",
           "description": "The URI path to be used for upload. Should be used in conjunction with the basePath property at the api-level."
          }
         }
        },
        "simple": {
         "type": "object",
         "description": "Supports uploading as a single HTTP request.",
         "properties": {
          "multipart": {
           "type": "boolean",
           "description": "True if this endpoint supports upload multipart media.",
           "default": "true"
          },
          "path": {
           "type": "string",
           "description": "The URI path to be used for upload. Should be used in conjunction with the basePath property at the api-level."
          }
         }
        }
       }
      }
     }
    },
    "parameterOrder": {
     "type": "array",
     "description": "Ordered list of required parameters, serves as a hint to clients on how to structure their method signatures. The array is ordered such that the \"most-significant\" parameter appears first.",
     "items": {
      "type": "string"
     }
    },
    "parameters": {
     "type": "object",
     "description": "Details for all parameters in this method.",
     "additionalProperties": {
      "$ref": "JsonSchema",
      "description": "Details for a single parameter in this method."
     }
    },
    "path": {
     "type": "string",
     "description": "The URI path of this REST method. Should be used in conjunction with the basePath property at the api-level."
    },
    "request": {
     "type": "object",
     "description": "The schema for the request.",
     "properties": {
      "$ref": {
       "type": "string",
       "description": "Schema ID for the request schema."
      }
     }
    },
    "response": {
     "type": "object",
     "description": "The schema for the response.",
     "properties": {
      "$ref": {
       "type": "string",
       "description": "Schema ID for the response schema."
      }
     }
    },
    "scopes": {
     "type": "array",
     "description": "OAuth 2.0 scopes applicable to this method.",
     "items": {
      "type": "any"
     }
    },
    "supportsMediaDownload": {
     "type": "boolean",
     "description": "Whether this method supports media downloads."
    },
    "supportsMediaUpload": {
     "type": "boolean",
     "description": "Whether this method supports media uploads."
    },
    "supportsSubscription": {
     "type": "boolean",
     "description": "Whether this method supports subscriptions."
    }
   }
  },
  "RestResource": {
   "id": "RestResource",
   "type": "object",
   "properties": {
    "methods": {
     "type": "object",
     "description": "Methods on this resource.",
     "additionalProperties": {
      "$ref": "RestMethod",
      "description": "Description for any methods on this resource."
     }
    },
    "resources": {
     "type": "object",
     "description": "Sub-resources on this resource.",
     "additionalProperties": {
      "$ref": "RestResource",
      "description": "Description for any sub-resources on this resource."
     }
    }
   }
  }
 },
 "resources": {
  "apis": {
   "methods": {
    "getRest": {
     "id": "discovery.apis.getRest",
     "path": "apis/{api}/{version}/rest",
     "httpMethod": "GET",
     "description": "Retrieve the description of a particular version of an api.",
     "parameters": {
      "api": {
       "type": "string",
       "description": "The name of the API.",
       "required": true,
       "location": "path"
      },
      "version": {
       "type": "string",
       "description": "The version of the API.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "api",
      "version"
     ],
     "response": {
      "$ref": "RestDescription"
     }
    },
    "list": {
     "id": "discovery.apis.list",
     "path": "apis",
     "httpMethod": "GET",
     "description": "Retrieve the list of APIs supported at this endpoint.",
     "parameters": {
      "label": {
       "type": "string",
       "description": "Only include APIs with a matching label, such as 'graduated' or 'labs'.",
       "enum": [
        "deprecated",
        "graduated",
        "labs"
       ],
       "enumDescriptions": [
        "APIs that have been deprecated.",
        "Supported APIs that have graduated from labs.",
        "APIs that are experimental"
       ],
       "location": "query"
      },
      "name": {
       "type": "string",
       "description": "Only include APIs with the given name.",
       "location": "query"
      },
      "preferred": {
       "type": "boolean",
       "description": "Return only the preferred version of an API.",
       "default": "false",
       "location": "query"
      }
     },
     "response": {
      "$ref": "DirectoryList"
     }
    }
   }
  }
 }
}
