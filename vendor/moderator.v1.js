{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "moderator:v1",
 "name": "moderator",
 "version": "v1",
 "revision": "19700115",
 "title": "Moderator API",
 "description": "Moderator API",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/moderator-32.png",
  "x32": "http://www.google.com/images/icons/product/search-32.gif"
 },
 "documentationLink": "http://code.google.com/apis/moderator/v1/using_rest.html",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/moderator/v1/",
 "basePath": "/moderator/v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "moderator/v1/",
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
    "https://www.googleapis.com/auth/moderator": {
     "description": "Manage your activity in Google Moderator"
    }
   }
  }
 },
 "features": [
  "dataWrapper"
 ],
 "schemas": {
  "ModeratorTopicsResourcePartial": {
   "id": "ModeratorTopicsResourcePartial",
   "type": "object",
   "properties": {
    "id": {
     "type": "object",
     "properties": {
      "seriesId": {
       "type": "string",
       "format": "int64"
      },
      "topicId": {
       "type": "string",
       "format": "int64"
      }
     }
    }
   }
  },
  "ModeratorVotesResourcePartial": {
   "id": "ModeratorVotesResourcePartial",
   "type": "object",
   "properties": {
    "flag": {
     "type": "string"
    },
    "vote": {
     "type": "string"
    }
   }
  },
  "Profile": {
   "id": "Profile",
   "type": "object",
   "properties": {
    "attribution": {
     "type": "object",
     "properties": {
      "avatarUrl": {
       "type": "string"
      },
      "displayName": {
       "type": "string"
      },
      "geo": {
       "type": "object",
       "properties": {
        "latitude": {
         "type": "number",
         "format": "double"
        },
        "location": {
         "type": "string"
        },
        "longitude": {
         "type": "number",
         "format": "double"
        }
       }
      },
      "location": {
       "type": "string"
      }
     }
    },
    "id": {
     "type": "object",
     "properties": {
      "user": {
       "type": "string"
      }
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#profile"
    }
   }
  },
  "Series": {
   "id": "Series",
   "type": "object",
   "properties": {
    "anonymousSubmissionAllowed": {
     "type": "boolean"
    },
    "counters": {
     "type": "object",
     "properties": {
      "anonymousSubmissions": {
       "type": "integer",
       "format": "int32"
      },
      "minusVotes": {
       "type": "integer",
       "format": "int32"
      },
      "noneVotes": {
       "type": "integer",
       "format": "int32"
      },
      "plusVotes": {
       "type": "integer",
       "format": "int32"
      },
      "submissions": {
       "type": "integer",
       "format": "int32"
      },
      "users": {
       "type": "integer",
       "format": "int32"
      },
      "videoSubmissions": {
       "type": "integer",
       "format": "int32"
      }
     }
    },
    "description": {
     "type": "string"
    },
    "id": {
     "type": "object",
     "properties": {
      "seriesId": {
       "type": "string",
       "format": "int64"
      }
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#series"
    },
    "name": {
     "type": "string"
    },
    "numTopics": {
     "type": "integer",
     "format": "int32"
    },
    "rules": {
     "type": "object",
     "properties": {
      "submissions": {
       "type": "object",
       "properties": {
        "close": {
         "type": "string",
         "format": "uint64"
        },
        "open": {
         "type": "string",
         "format": "uint64"
        }
       }
      },
      "votes": {
       "type": "object",
       "properties": {
        "close": {
         "type": "string",
         "format": "uint64"
        },
        "open": {
         "type": "string",
         "format": "uint64"
        }
       }
      }
     }
    },
    "unauthSubmissionAllowed": {
     "type": "boolean"
    },
    "unauthVotingAllowed": {
     "type": "boolean"
    },
    "videoSubmissionAllowed": {
     "type": "boolean"
    }
   }
  },
  "SeriesList": {
   "id": "SeriesList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "items": {
      "$ref": "Series"
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#seriesList"
    }
   }
  },
  "Submission": {
   "id": "Submission",
   "type": "object",
   "properties": {
    "attachmentUrl": {
     "type": "string"
    },
    "attribution": {
     "type": "object",
     "properties": {
      "avatarUrl": {
       "type": "string"
      },
      "displayName": {
       "type": "string"
      },
      "location": {
       "type": "string"
      }
     }
    },
    "author": {
     "type": "string"
    },
    "counters": {
     "type": "object",
     "properties": {
      "minusVotes": {
       "type": "integer",
       "format": "int32"
      },
      "noneVotes": {
       "type": "integer",
       "format": "int32"
      },
      "plusVotes": {
       "type": "integer",
       "format": "int32"
      }
     }
    },
    "created": {
     "type": "string",
     "format": "uint64"
    },
    "geo": {
     "type": "object",
     "properties": {
      "latitude": {
       "type": "number",
       "format": "double"
      },
      "location": {
       "type": "string"
      },
      "longitude": {
       "type": "number",
       "format": "double"
      }
     }
    },
    "id": {
     "type": "object",
     "properties": {
      "seriesId": {
       "type": "string",
       "format": "int64"
      },
      "submissionId": {
       "type": "string",
       "format": "int64"
      }
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#submission"
    },
    "parentSubmissionId": {
     "type": "object",
     "properties": {
      "seriesId": {
       "type": "string",
       "format": "int64"
      },
      "submissionId": {
       "type": "string",
       "format": "int64"
      }
     }
    },
    "text": {
     "type": "string"
    },
    "topics": {
     "type": "array",
     "items": {
      "$ref": "ModeratorTopicsResourcePartial"
     }
    },
    "translations": {
     "type": "array",
     "items": {
      "type": "object",
      "properties": {
       "lang": {
        "type": "string"
       },
       "text": {
        "type": "string"
       }
      }
     }
    },
    "vote": {
     "$ref": "ModeratorVotesResourcePartial"
    }
   }
  },
  "SubmissionList": {
   "id": "SubmissionList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "items": {
      "$ref": "Submission"
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#submissionList"
    }
   }
  },
  "Tag": {
   "id": "Tag",
   "type": "object",
   "properties": {
    "id": {
     "type": "object",
     "properties": {
      "seriesId": {
       "type": "string",
       "format": "int64"
      },
      "submissionId": {
       "type": "string",
       "format": "int64"
      },
      "tagId": {
       "type": "string"
      }
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#tag"
    },
    "text": {
     "type": "string"
    }
   }
  },
  "TagList": {
   "id": "TagList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "items": {
      "$ref": "Tag"
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#tagList"
    }
   }
  },
  "Topic": {
   "id": "Topic",
   "type": "object",
   "properties": {
    "counters": {
     "type": "object",
     "properties": {
      "minusVotes": {
       "type": "integer",
       "format": "int32"
      },
      "noneVotes": {
       "type": "integer",
       "format": "int32"
      },
      "plusVotes": {
       "type": "integer",
       "format": "int32"
      },
      "submissions": {
       "type": "integer",
       "format": "int32"
      },
      "users": {
       "type": "integer",
       "format": "int32"
      },
      "videoSubmissions": {
       "type": "integer",
       "format": "int32"
      }
     }
    },
    "description": {
     "type": "string"
    },
    "featuredSubmission": {
     "$ref": "Submission"
    },
    "id": {
     "type": "object",
     "properties": {
      "seriesId": {
       "type": "string",
       "format": "int64"
      },
      "topicId": {
       "type": "string",
       "format": "int64"
      }
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#topic"
    },
    "name": {
     "type": "string"
    },
    "presenter": {
     "type": "string"
    },
    "rules": {
     "type": "object",
     "properties": {
      "submissions": {
       "type": "object",
       "properties": {
        "close": {
         "type": "string",
         "format": "uint64"
        },
        "open": {
         "type": "string",
         "format": "uint64"
        }
       }
      },
      "votes": {
       "type": "object",
       "properties": {
        "close": {
         "type": "string",
         "format": "uint64"
        },
        "open": {
         "type": "string",
         "format": "uint64"
        }
       }
      }
     }
    }
   }
  },
  "TopicList": {
   "id": "TopicList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "items": {
      "$ref": "Topic"
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#topicList"
    }
   }
  },
  "Vote": {
   "id": "Vote",
   "type": "object",
   "properties": {
    "flag": {
     "type": "string"
    },
    "id": {
     "type": "object",
     "properties": {
      "seriesId": {
       "type": "string",
       "format": "int64"
      },
      "submissionId": {
       "type": "string",
       "format": "int64"
      }
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#vote"
    },
    "vote": {
     "type": "string"
    }
   }
  },
  "VoteList": {
   "id": "VoteList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "items": {
      "$ref": "Vote"
     }
    },
    "kind": {
     "type": "string",
     "default": "moderator#voteList"
    }
   }
  }
 },
 "resources": {
  "featured": {
   "resources": {
    "series": {
     "methods": {
      "list": {
       "id": "moderator.featured.series.list",
       "path": "series/featured",
       "httpMethod": "GET",
       "description": "Lists the featured series.",
       "response": {
        "$ref": "SeriesList"
       },
       "scopes": [
        "https://www.googleapis.com/auth/moderator"
       ]
      }
     }
    }
   }
  },
  "global": {
   "resources": {
    "series": {
     "methods": {
      "list": {
       "id": "moderator.global.series.list",
       "path": "search",
       "httpMethod": "GET",
       "description": "Searches the public series and returns the search results.",
       "parameters": {
        "max-results": {
         "type": "integer",
         "description": "Maximum number of results to return.",
         "format": "uint32",
         "location": "query"
        },
        "q": {
         "type": "string",
         "description": "Search query.",
         "location": "query"
        },
        "start-index": {
         "type": "integer",
         "description": "Index of the first result to be retrieved.",
         "format": "uint32",
         "location": "query"
        }
       },
       "response": {
        "$ref": "SeriesList"
       },
       "scopes": [
        "https://www.googleapis.com/auth/moderator"
       ]
      }
     }
    }
   }
  },
  "my": {
   "resources": {
    "series": {
     "methods": {
      "list": {
       "id": "moderator.my.series.list",
       "path": "series/@me/mine",
       "httpMethod": "GET",
       "description": "Lists all series created by the authenticated user.",
       "response": {
        "$ref": "SeriesList"
       },
       "scopes": [
        "https://www.googleapis.com/auth/moderator"
       ]
      }
     }
    }
   }
  },
  "myrecent": {
   "resources": {
    "series": {
     "methods": {
      "list": {
       "id": "moderator.myrecent.series.list",
       "path": "series/@me/recent",
       "httpMethod": "GET",
       "description": "Lists the series the authenticated user has visited.",
       "response": {
        "$ref": "SeriesList"
       },
       "scopes": [
        "https://www.googleapis.com/auth/moderator"
       ]
      }
     }
    }
   }
  },
  "profiles": {
   "methods": {
    "get": {
     "id": "moderator.profiles.get",
     "path": "profiles/@me",
     "httpMethod": "GET",
     "description": "Returns the profile information for the authenticated user.",
     "response": {
      "$ref": "Profile"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "patch": {
     "id": "moderator.profiles.patch",
     "path": "profiles/@me",
     "httpMethod": "PATCH",
     "description": "Updates the profile information for the authenticated user. This method supports patch semantics.",
     "request": {
      "$ref": "Profile"
     },
     "response": {
      "$ref": "Profile"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "update": {
     "id": "moderator.profiles.update",
     "path": "profiles/@me",
     "httpMethod": "PUT",
     "description": "Updates the profile information for the authenticated user.",
     "request": {
      "$ref": "Profile"
     },
     "response": {
      "$ref": "Profile"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    }
   }
  },
  "responses": {
   "methods": {
    "insert": {
     "id": "moderator.responses.insert",
     "path": "series/{seriesId}/topics/{topicId}/submissions/{parentSubmissionId}/responses",
     "httpMethod": "POST",
     "description": "Inserts a response for the specified submission in the specified topic within the specified series.",
     "parameters": {
      "anonymous": {
       "type": "boolean",
       "description": "Set to true to mark the new submission as anonymous.",
       "location": "query"
      },
      "parentSubmissionId": {
       "type": "integer",
       "description": "The decimal ID of the parent Submission within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "topicId": {
       "type": "integer",
       "description": "The decimal ID of the Topic within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "unauthToken": {
       "type": "string",
       "description": "User identifier for unauthenticated usage mode",
       "location": "query"
      }
     },
     "parameterOrder": [
      "seriesId",
      "topicId",
      "parentSubmissionId"
     ],
     "request": {
      "$ref": "Submission"
     },
     "response": {
      "$ref": "Submission"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "list": {
     "id": "moderator.responses.list",
     "path": "series/{seriesId}/submissions/{submissionId}/responses",
     "httpMethod": "GET",
     "description": "Lists or searches the responses for the specified submission within the specified series and returns the search results.",
     "parameters": {
      "author": {
       "type": "string",
       "description": "Restricts the results to submissions by a specific author.",
       "location": "query"
      },
      "hasAttachedVideo": {
       "type": "boolean",
       "description": "Specifies whether to restrict to submissions that have videos attached.",
       "location": "query"
      },
      "max-results": {
       "type": "integer",
       "description": "Maximum number of results to return.",
       "format": "uint32",
       "location": "query"
      },
      "q": {
       "type": "string",
       "description": "Search query.",
       "location": "query"
      },
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "sort": {
       "type": "string",
       "description": "Sort order.",
       "location": "query"
      },
      "start-index": {
       "type": "integer",
       "description": "Index of the first result to be retrieved.",
       "format": "uint32",
       "location": "query"
      },
      "submissionId": {
       "type": "integer",
       "description": "The decimal ID of the Submission within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId",
      "submissionId"
     ],
     "response": {
      "$ref": "SubmissionList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    }
   }
  },
  "series": {
   "methods": {
    "get": {
     "id": "moderator.series.get",
     "path": "series/{seriesId}",
     "httpMethod": "GET",
     "description": "Returns the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId"
     ],
     "response": {
      "$ref": "Series"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "insert": {
     "id": "moderator.series.insert",
     "path": "series",
     "httpMethod": "POST",
     "description": "Inserts a new series.",
     "request": {
      "$ref": "Series"
     },
     "response": {
      "$ref": "Series"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "list": {
     "id": "moderator.series.list",
     "path": "series",
     "httpMethod": "GET",
     "description": "Searches the series and returns the search results.",
     "parameters": {
      "max-results": {
       "type": "integer",
       "description": "Maximum number of results to return.",
       "format": "uint32",
       "location": "query"
      },
      "q": {
       "type": "string",
       "description": "Search query.",
       "location": "query"
      },
      "start-index": {
       "type": "integer",
       "description": "Index of the first result to be retrieved.",
       "format": "uint32",
       "location": "query"
      }
     },
     "response": {
      "$ref": "SeriesList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "patch": {
     "id": "moderator.series.patch",
     "path": "series/{seriesId}",
     "httpMethod": "PATCH",
     "description": "Updates the specified series. This method supports patch semantics.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId"
     ],
     "request": {
      "$ref": "Series"
     },
     "response": {
      "$ref": "Series"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "update": {
     "id": "moderator.series.update",
     "path": "series/{seriesId}",
     "httpMethod": "PUT",
     "description": "Updates the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId"
     ],
     "request": {
      "$ref": "Series"
     },
     "response": {
      "$ref": "Series"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    }
   },
   "resources": {
    "responses": {
     "methods": {
      "list": {
       "id": "moderator.series.responses.list",
       "path": "series/{seriesId}/responses",
       "httpMethod": "GET",
       "description": "Searches the responses for the specified series and returns the search results.",
       "parameters": {
        "author": {
         "type": "string",
         "description": "Restricts the results to submissions by a specific author.",
         "location": "query"
        },
        "hasAttachedVideo": {
         "type": "boolean",
         "description": "Specifies whether to restrict to submissions that have videos attached.",
         "location": "query"
        },
        "max-results": {
         "type": "integer",
         "description": "Maximum number of results to return.",
         "format": "uint32",
         "location": "query"
        },
        "q": {
         "type": "string",
         "description": "Search query.",
         "location": "query"
        },
        "seriesId": {
         "type": "integer",
         "description": "The decimal ID of the Series.",
         "required": true,
         "format": "uint32",
         "location": "path"
        },
        "sort": {
         "type": "string",
         "description": "Sort order.",
         "location": "query"
        },
        "start-index": {
         "type": "integer",
         "description": "Index of the first result to be retrieved.",
         "format": "uint32",
         "location": "query"
        }
       },
       "parameterOrder": [
        "seriesId"
       ],
       "response": {
        "$ref": "SeriesList"
       },
       "scopes": [
        "https://www.googleapis.com/auth/moderator"
       ]
      }
     }
    },
    "submissions": {
     "methods": {
      "list": {
       "id": "moderator.series.submissions.list",
       "path": "series/{seriesId}/submissions",
       "httpMethod": "GET",
       "description": "Searches the submissions for the specified series and returns the search results.",
       "parameters": {
        "author": {
         "type": "string",
         "description": "Restricts the results to submissions by a specific author.",
         "location": "query"
        },
        "hasAttachedVideo": {
         "type": "boolean",
         "description": "Specifies whether to restrict to submissions that have videos attached.",
         "location": "query"
        },
        "includeVotes": {
         "type": "boolean",
         "description": "Specifies whether to include the current user's vote",
         "location": "query"
        },
        "lang": {
         "type": "string",
         "description": "The language code for the language the client prefers resuls in.",
         "location": "query"
        },
        "max-results": {
         "type": "integer",
         "description": "Maximum number of results to return.",
         "format": "uint32",
         "location": "query"
        },
        "q": {
         "type": "string",
         "description": "Search query.",
         "location": "query"
        },
        "seriesId": {
         "type": "integer",
         "description": "The decimal ID of the Series.",
         "required": true,
         "format": "uint32",
         "location": "path"
        },
        "sort": {
         "type": "string",
         "description": "Sort order.",
         "location": "query"
        },
        "start-index": {
         "type": "integer",
         "description": "Index of the first result to be retrieved.",
         "format": "uint32",
         "location": "query"
        }
       },
       "parameterOrder": [
        "seriesId"
       ],
       "response": {
        "$ref": "SubmissionList"
       },
       "scopes": [
        "https://www.googleapis.com/auth/moderator"
       ]
      }
     }
    }
   }
  },
  "submissions": {
   "methods": {
    "get": {
     "id": "moderator.submissions.get",
     "path": "series/{seriesId}/submissions/{submissionId}",
     "httpMethod": "GET",
     "description": "Returns the specified submission within the specified series.",
     "parameters": {
      "includeVotes": {
       "type": "boolean",
       "description": "Specifies whether to include the current user's vote",
       "location": "query"
      },
      "lang": {
       "type": "string",
       "description": "The language code for the language the client prefers resuls in.",
       "location": "query"
      },
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "submissionId": {
       "type": "integer",
       "description": "The decimal ID of the Submission within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId",
      "submissionId"
     ],
     "response": {
      "$ref": "Submission"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "insert": {
     "id": "moderator.submissions.insert",
     "path": "series/{seriesId}/topics/{topicId}/submissions",
     "httpMethod": "POST",
     "description": "Inserts a new submission in the specified topic within the specified series.",
     "parameters": {
      "anonymous": {
       "type": "boolean",
       "description": "Set to true to mark the new submission as anonymous.",
       "location": "query"
      },
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "topicId": {
       "type": "integer",
       "description": "The decimal ID of the Topic within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "unauthToken": {
       "type": "string",
       "description": "User identifier for unauthenticated usage mode",
       "location": "query"
      }
     },
     "parameterOrder": [
      "seriesId",
      "topicId"
     ],
     "request": {
      "$ref": "Submission"
     },
     "response": {
      "$ref": "Submission"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    }
   }
  },
  "tags": {
   "methods": {
    "delete": {
     "id": "moderator.tags.delete",
     "path": "series/{seriesId}/submissions/{submissionId}/tags/{tagId}",
     "httpMethod": "DELETE",
     "description": "Deletes the specified tag from the specified submission within the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "submissionId": {
       "type": "integer",
       "description": "The decimal ID of the Submission within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "tagId": {
       "type": "string",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId",
      "submissionId",
      "tagId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "insert": {
     "id": "moderator.tags.insert",
     "path": "series/{seriesId}/submissions/{submissionId}/tags",
     "httpMethod": "POST",
     "description": "Inserts a new tag for the specified submission within the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "submissionId": {
       "type": "integer",
       "description": "The decimal ID of the Submission within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId",
      "submissionId"
     ],
     "request": {
      "$ref": "Tag"
     },
     "response": {
      "$ref": "Tag"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "list": {
     "id": "moderator.tags.list",
     "path": "series/{seriesId}/submissions/{submissionId}/tags",
     "httpMethod": "GET",
     "description": "Lists all tags for the specified submission within the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "submissionId": {
       "type": "integer",
       "description": "The decimal ID of the Submission within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId",
      "submissionId"
     ],
     "response": {
      "$ref": "TagList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    }
   }
  },
  "topics": {
   "methods": {
    "get": {
     "id": "moderator.topics.get",
     "path": "series/{seriesId}/topics/{topicId}",
     "httpMethod": "GET",
     "description": "Returns the specified topic from the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "topicId": {
       "type": "integer",
       "description": "The decimal ID of the Topic within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId",
      "topicId"
     ],
     "response": {
      "$ref": "Topic"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "insert": {
     "id": "moderator.topics.insert",
     "path": "series/{seriesId}/topics",
     "httpMethod": "POST",
     "description": "Inserts a new topic into the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId"
     ],
     "request": {
      "$ref": "Topic"
     },
     "response": {
      "$ref": "Topic"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "list": {
     "id": "moderator.topics.list",
     "path": "series/{seriesId}/topics",
     "httpMethod": "GET",
     "description": "Searches the topics within the specified series and returns the search results.",
     "parameters": {
      "max-results": {
       "type": "integer",
       "description": "Maximum number of results to return.",
       "format": "uint32",
       "location": "query"
      },
      "mode": {
       "type": "string",
       "location": "query"
      },
      "q": {
       "type": "string",
       "description": "Search query.",
       "location": "query"
      },
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "start-index": {
       "type": "integer",
       "description": "Index of the first result to be retrieved.",
       "format": "uint32",
       "location": "query"
      }
     },
     "parameterOrder": [
      "seriesId"
     ],
     "response": {
      "$ref": "TopicList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "update": {
     "id": "moderator.topics.update",
     "path": "series/{seriesId}/topics/{topicId}",
     "httpMethod": "PUT",
     "description": "Updates the specified topic within the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "topicId": {
       "type": "integer",
       "description": "The decimal ID of the Topic within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "seriesId",
      "topicId"
     ],
     "request": {
      "$ref": "Topic"
     },
     "response": {
      "$ref": "Topic"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    }
   },
   "resources": {
    "submissions": {
     "methods": {
      "list": {
       "id": "moderator.topics.submissions.list",
       "path": "series/{seriesId}/topics/{topicId}/submissions",
       "httpMethod": "GET",
       "description": "Searches the submissions for the specified topic within the specified series and returns the search results.",
       "parameters": {
        "author": {
         "type": "string",
         "description": "Restricts the results to submissions by a specific author.",
         "location": "query"
        },
        "hasAttachedVideo": {
         "type": "boolean",
         "description": "Specifies whether to restrict to submissions that have videos attached.",
         "location": "query"
        },
        "includeVotes": {
         "type": "boolean",
         "description": "Specifies whether to include the current user's vote",
         "location": "query"
        },
        "max-results": {
         "type": "integer",
         "description": "Maximum number of results to return.",
         "format": "uint32",
         "location": "query"
        },
        "q": {
         "type": "string",
         "description": "Search query.",
         "location": "query"
        },
        "seriesId": {
         "type": "integer",
         "description": "The decimal ID of the Series.",
         "required": true,
         "format": "uint32",
         "location": "path"
        },
        "sort": {
         "type": "string",
         "description": "Sort order.",
         "location": "query"
        },
        "start-index": {
         "type": "integer",
         "description": "Index of the first result to be retrieved.",
         "format": "uint32",
         "location": "query"
        },
        "topicId": {
         "type": "integer",
         "description": "The decimal ID of the Topic within the Series.",
         "required": true,
         "format": "uint32",
         "location": "path"
        }
       },
       "parameterOrder": [
        "seriesId",
        "topicId"
       ],
       "response": {
        "$ref": "SubmissionList"
       },
       "scopes": [
        "https://www.googleapis.com/auth/moderator"
       ]
      }
     }
    }
   }
  },
  "votes": {
   "methods": {
    "get": {
     "id": "moderator.votes.get",
     "path": "series/{seriesId}/submissions/{submissionId}/votes/@me",
     "httpMethod": "GET",
     "description": "Returns the votes by the authenticated user for the specified submission within the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "submissionId": {
       "type": "integer",
       "description": "The decimal ID of the Submission within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "unauthToken": {
       "type": "string",
       "description": "User identifier for unauthenticated usage mode",
       "location": "query"
      },
      "userId": {
       "type": "string",
       "location": "query"
      }
     },
     "parameterOrder": [
      "seriesId",
      "submissionId"
     ],
     "response": {
      "$ref": "Vote"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "insert": {
     "id": "moderator.votes.insert",
     "path": "series/{seriesId}/submissions/{submissionId}/votes/@me",
     "httpMethod": "POST",
     "description": "Inserts a new vote by the authenticated user for the specified submission within the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "submissionId": {
       "type": "integer",
       "description": "The decimal ID of the Submission within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "unauthToken": {
       "type": "string",
       "description": "User identifier for unauthenticated usage mode",
       "location": "query"
      }
     },
     "parameterOrder": [
      "seriesId",
      "submissionId"
     ],
     "request": {
      "$ref": "Vote"
     },
     "response": {
      "$ref": "Vote"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "list": {
     "id": "moderator.votes.list",
     "path": "series/{seriesId}/votes/@me",
     "httpMethod": "GET",
     "description": "Lists the votes by the authenticated user for the given series.",
     "parameters": {
      "max-results": {
       "type": "integer",
       "description": "Maximum number of results to return.",
       "format": "uint32",
       "location": "query"
      },
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "start-index": {
       "type": "integer",
       "description": "Index of the first result to be retrieved.",
       "format": "uint32",
       "location": "query"
      }
     },
     "parameterOrder": [
      "seriesId"
     ],
     "response": {
      "$ref": "VoteList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "patch": {
     "id": "moderator.votes.patch",
     "path": "series/{seriesId}/submissions/{submissionId}/votes/@me",
     "httpMethod": "PATCH",
     "description": "Updates the votes by the authenticated user for the specified submission within the specified series. This method supports patch semantics.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "submissionId": {
       "type": "integer",
       "description": "The decimal ID of the Submission within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "unauthToken": {
       "type": "string",
       "description": "User identifier for unauthenticated usage mode",
       "location": "query"
      },
      "userId": {
       "type": "string",
       "location": "query"
      }
     },
     "parameterOrder": [
      "seriesId",
      "submissionId"
     ],
     "request": {
      "$ref": "Vote"
     },
     "response": {
      "$ref": "Vote"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    },
    "update": {
     "id": "moderator.votes.update",
     "path": "series/{seriesId}/submissions/{submissionId}/votes/@me",
     "httpMethod": "PUT",
     "description": "Updates the votes by the authenticated user for the specified submission within the specified series.",
     "parameters": {
      "seriesId": {
       "type": "integer",
       "description": "The decimal ID of the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "submissionId": {
       "type": "integer",
       "description": "The decimal ID of the Submission within the Series.",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "unauthToken": {
       "type": "string",
       "description": "User identifier for unauthenticated usage mode",
       "location": "query"
      },
      "userId": {
       "type": "string",
       "location": "query"
      }
     },
     "parameterOrder": [
      "seriesId",
      "submissionId"
     ],
     "request": {
      "$ref": "Vote"
     },
     "response": {
      "$ref": "Vote"
     },
     "scopes": [
      "https://www.googleapis.com/auth/moderator"
     ]
    }
   }
  }
 }
}
