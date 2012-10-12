{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "books:v1",
 "name": "books",
 "version": "v1",
 "revision": "20120822",
 "title": "Books API",
 "description": "Lets you search for books and manage your Google Books library.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/ebooks-16.png",
  "x32": "http://www.google.com/images/icons/product/ebooks-32.png"
 },
 "documentationLink": "https://developers.google.com/books/docs/v1/getting_started",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/books/v1/",
 "basePath": "/books/v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "books/v1/",
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
    "https://www.googleapis.com/auth/books": {
     "description": "Manage your books"
    }
   }
  }
 },
 "schemas": {
  "Annotation": {
   "id": "Annotation",
   "type": "object",
   "properties": {
    "afterSelectedText": {
     "type": "string",
     "description": "Anchor text after excerpt."
    },
    "beforeSelectedText": {
     "type": "string",
     "description": "Anchor text before excerpt."
    },
    "clientVersionRanges": {
     "type": "object",
     "description": "Selection ranges sent from the client.",
     "properties": {
      "cfiRange": {
       "$ref": "BooksAnnotationsRange",
       "description": "Range in CFI format for this annotation sent by client."
      },
      "contentVersion": {
       "type": "string",
       "description": "Content version the client sent in."
      },
      "gbImageRange": {
       "$ref": "BooksAnnotationsRange",
       "description": "Range in GB image format for this annotation sent by client."
      },
      "gbTextRange": {
       "$ref": "BooksAnnotationsRange",
       "description": "Range in GB text format for this annotation sent by client."
      }
     }
    },
    "created": {
     "type": "string",
     "description": "Timestamp for the created time of this annotation.",
     "format": "date-time"
    },
    "currentVersionRanges": {
     "type": "object",
     "description": "Selection ranges for the most recent content version.",
     "properties": {
      "cfiRange": {
       "$ref": "BooksAnnotationsRange",
       "description": "Range in CFI format for this annotation for version above."
      },
      "contentVersion": {
       "type": "string",
       "description": "Content version applicable to ranges below."
      },
      "gbImageRange": {
       "$ref": "BooksAnnotationsRange",
       "description": "Range in GB image format for this annotation for version above."
      },
      "gbTextRange": {
       "$ref": "BooksAnnotationsRange",
       "description": "Range in GB text format for this annotation for version above."
      }
     }
    },
    "data": {
     "type": "string",
     "description": "User-created data for this annotation."
    },
    "deleted": {
     "type": "boolean",
     "description": "Indicates that this annotation is deleted."
    },
    "highlightStyle": {
     "type": "string",
     "description": "The highlight style for this annotation."
    },
    "id": {
     "type": "string",
     "description": "Id of this annotation, in the form of a GUID."
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "books#annotation"
    },
    "layerId": {
     "type": "string",
     "description": "The layer this annotation is for."
    },
    "pageIds": {
     "type": "array",
     "description": "Pages that this annotation spans.",
     "items": {
      "type": "string"
     }
    },
    "selectedText": {
     "type": "string",
     "description": "Excerpt from the volume."
    },
    "selfLink": {
     "type": "string",
     "description": "URL to this resource."
    },
    "updated": {
     "type": "string",
     "description": "Timestamp for the last time this annotation was modified.",
     "format": "date-time"
    },
    "volumeId": {
     "type": "string",
     "description": "The volume that this annotation belongs to."
    }
   }
  },
  "Annotationdata": {
   "id": "Annotationdata",
   "type": "object",
   "properties": {
    "annotationType": {
     "type": "string",
     "description": "The type of annotation this data is for."
    },
    "data": {
     "type": "any"
    },
    "encoded_data": {
     "type": "string",
     "description": "Base64 encoded data for this annotation data.",
     "format": "byte"
    },
    "id": {
     "type": "string",
     "description": "Unique id for this annotation data."
    },
    "kind": {
     "type": "string",
     "description": "Resource Type",
     "default": "books#annotationdata"
    },
    "layerId": {
     "type": "string",
     "description": "The Layer id for this data. *"
    },
    "selfLink": {
     "type": "string",
     "description": "URL for this resource. *"
    },
    "updated": {
     "type": "string",
     "description": "Timestamp for the last time this data was updated. (RFC 3339 UTC date-time format).",
     "format": "date-time"
    },
    "volumeId": {
     "type": "string",
     "description": "The volume id for this data. *"
    }
   }
  },
  "Annotations": {
   "id": "Annotations",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of annotations.",
     "items": {
      "$ref": "Annotation"
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "books#annotations"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token to pass in for pagination for the next page. This will not be present if this request does not have more results."
    },
    "totalItems": {
     "type": "integer",
     "description": "Total number of annotations found. This may be greater than the number of notes returned in this response if results have been paginated.",
     "format": "int32"
    }
   }
  },
  "Annotationsdata": {
   "id": "Annotationsdata",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of Annotation Data.",
     "items": {
      "$ref": "Annotationdata"
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type",
     "default": "books#annotationsdata"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token to pass in for pagination for the next page. This will not be present if this request does not have more results."
    },
    "totalItems": {
     "type": "integer",
     "description": "The total number of volume annotations found.",
     "format": "int32"
    }
   }
  },
  "BooksAnnotationsRange": {
   "id": "BooksAnnotationsRange",
   "type": "object",
   "properties": {
    "endOffset": {
     "type": "string",
     "description": "The offset from the ending position."
    },
    "endPosition": {
     "type": "string",
     "description": "The ending position for the range."
    },
    "startOffset": {
     "type": "string",
     "description": "The offset from the starting position."
    },
    "startPosition": {
     "type": "string",
     "description": "The starting position for the range."
    }
   }
  },
  "BooksLayerDictData": {
   "id": "BooksLayerDictData",
   "type": "object",
   "properties": {
    "common": {
     "type": "object",
     "properties": {
      "title": {
       "type": "string",
       "description": "The display title and localized canonical name to use when searching for this entity on Google search."
      }
     }
    },
    "dict": {
     "type": "object",
     "properties": {
      "source": {
       "type": "object",
       "description": "The source, url and attribution for this dictionary data.",
       "properties": {
        "attribution": {
         "type": "string"
        },
        "url": {
         "type": "string"
        }
       }
      },
      "words": {
       "type": "array",
       "items": {
        "type": "object",
        "properties": {
         "derivatives": {
          "type": "array",
          "items": {
           "type": "object",
           "properties": {
            "source": {
             "type": "object",
             "properties": {
              "attribution": {
               "type": "string"
              },
              "url": {
               "type": "string"
              }
             }
            },
            "text": {
             "type": "string"
            }
           }
          }
         },
         "examples": {
          "type": "array",
          "items": {
           "type": "object",
           "properties": {
            "source": {
             "type": "object",
             "properties": {
              "attribution": {
               "type": "string"
              },
              "url": {
               "type": "string"
              }
             }
            },
            "text": {
             "type": "string"
            }
           }
          }
         },
         "senses": {
          "type": "array",
          "items": {
           "type": "object",
           "properties": {
            "conjugations": {
             "type": "array",
             "items": {
              "type": "object",
              "properties": {
               "type": {
                "type": "string"
               },
               "value": {
                "type": "string"
               }
              }
             }
            },
            "definitions": {
             "type": "array",
             "items": {
              "type": "object",
              "properties": {
               "definition": {
                "type": "string"
               },
               "examples": {
                "type": "array",
                "items": {
                 "type": "object",
                 "properties": {
                  "source": {
                   "type": "object",
                   "properties": {
                    "attribution": {
                     "type": "string"
                    },
                    "url": {
                     "type": "string"
                    }
                   }
                  },
                  "text": {
                   "type": "string"
                  }
                 }
                }
               }
              }
             }
            },
            "partOfSpeech": {
             "type": "string"
            },
            "pronunciation": {
             "type": "string"
            },
            "pronunciationUrl": {
             "type": "string"
            },
            "source": {
             "type": "object",
             "properties": {
              "attribution": {
               "type": "string"
              },
              "url": {
               "type": "string"
              }
             }
            },
            "syllabification": {
             "type": "string"
            },
            "synonyms": {
             "type": "array",
             "items": {
              "type": "object",
              "properties": {
               "source": {
                "type": "object",
                "properties": {
                 "attribution": {
                  "type": "string"
                 },
                 "url": {
                  "type": "string"
                 }
                }
               },
               "text": {
                "type": "string"
               }
              }
             }
            }
           }
          }
         },
         "source": {
          "type": "object",
          "description": "The words with different meanings but not related words, e.g. \"go\" (game) and \"go\" (verb).",
          "properties": {
           "attribution": {
            "type": "string"
           },
           "url": {
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
  },
  "BooksLayerGeoData": {
   "id": "BooksLayerGeoData",
   "type": "object",
   "properties": {
    "common": {
     "type": "object",
     "properties": {
      "lang": {
       "type": "string",
       "description": "The language of the information url and description."
      },
      "previewImageUrl": {
       "type": "string",
       "description": "The URL for the preview image information."
      },
      "snippet": {
       "type": "string",
       "description": "The description for this location."
      },
      "snippetUrl": {
       "type": "string",
       "description": "The URL for information for this location. Ex: wikipedia link."
      },
      "title": {
       "type": "string",
       "description": "The display title and localized canonical name to use when searching for this entity on Google search."
      }
     }
    },
    "geo": {
     "type": "object",
     "properties": {
      "boundary": {
       "type": "array",
       "description": "The boundary of the location as a set of loops containing pairs of latitude, longitude coordinates.",
       "items": {
        "type": "array",
        "items": {
         "type": "object",
         "properties": {
          "latitude": {
           "type": "integer",
           "format": "uint32"
          },
          "longitude": {
           "type": "integer",
           "format": "uint32"
          }
         }
        }
       }
      },
      "cachePolicy": {
       "type": "string",
       "description": "The cache policy active for this data. EX: UNRESTRICTED, RESTRICTED, NEVER"
      },
      "countryCode": {
       "type": "string",
       "description": "The country code of the location."
      },
      "latitude": {
       "type": "number",
       "description": "The latitude of the location.",
       "format": "double"
      },
      "longitude": {
       "type": "number",
       "description": "The longitude of the location.",
       "format": "double"
      },
      "mapType": {
       "type": "string",
       "description": "The type of map that should be used for this location. EX: HYBRID, ROADMAP, SATELLITE, TERRAIN"
      },
      "viewport": {
       "type": "object",
       "description": "The viewport for showing this location. This is a latitude, longitude rectangle.",
       "properties": {
        "hi": {
         "type": "object",
         "properties": {
          "latitude": {
           "type": "number",
           "format": "double"
          },
          "longitude": {
           "type": "number",
           "format": "double"
          }
         }
        },
        "lo": {
         "type": "object",
         "properties": {
          "latitude": {
           "type": "number",
           "format": "double"
          },
          "longitude": {
           "type": "number",
           "format": "double"
          }
         }
        }
       }
      },
      "zoom": {
       "type": "integer",
       "description": "The Zoom level to use for the map. Zoom levels between 0 (the lowest zoom level, in which the entire world can be seen on one map) to 21+ (down to individual buildings). See: https://developers.google.com/maps/documentation/staticmaps/#Zoomlevels",
       "format": "int32"
      }
     }
    }
   }
  },
  "Bookshelf": {
   "id": "Bookshelf",
   "type": "object",
   "properties": {
    "access": {
     "type": "string",
     "description": "Whether this bookshelf is PUBLIC or PRIVATE."
    },
    "created": {
     "type": "string",
     "description": "Created time for this bookshelf (formatted UTC timestamp with millisecond resolution).",
     "format": "date-time"
    },
    "description": {
     "type": "string",
     "description": "Description of this bookshelf."
    },
    "id": {
     "type": "integer",
     "description": "Id of this bookshelf, only unique by user.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Resource type for bookshelf metadata.",
     "default": "books#bookshelf"
    },
    "selfLink": {
     "type": "string",
     "description": "URL to this resource."
    },
    "title": {
     "type": "string",
     "description": "Title of this bookshelf."
    },
    "updated": {
     "type": "string",
     "description": "Last modified time of this bookshelf (formatted UTC timestamp with millisecond resolution).",
     "format": "date-time"
    },
    "volumeCount": {
     "type": "integer",
     "description": "Number of volumes in this bookshelf.",
     "format": "int32"
    },
    "volumesLastUpdated": {
     "type": "string",
     "description": "Last time a volume was added or removed from this bookshelf (formatted UTC timestamp with millisecond resolution).",
     "format": "date-time"
    }
   }
  },
  "Bookshelves": {
   "id": "Bookshelves",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of bookshelves.",
     "items": {
      "$ref": "Bookshelf"
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "books#bookshelves"
    }
   }
  },
  "ConcurrentAccessRestriction": {
   "id": "ConcurrentAccessRestriction",
   "type": "object",
   "properties": {
    "deviceAllowed": {
     "type": "boolean",
     "description": "Whether access is granted for this (user, device, volume)."
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "books#concurrentAccessRestriction"
    },
    "maxConcurrentDevices": {
     "type": "integer",
     "description": "The maximum number of concurrent access licenses for this volume.",
     "format": "int32"
    },
    "message": {
     "type": "string",
     "description": "Error/warning message."
    },
    "nonce": {
     "type": "string",
     "description": "Client nonce for verification. Download access and client-validation only."
    },
    "reasonCode": {
     "type": "string",
     "description": "Error/warning reason code."
    },
    "restricted": {
     "type": "boolean",
     "description": "Whether this volume has any concurrent access restrictions."
    },
    "signature": {
     "type": "string",
     "description": "Response signature."
    },
    "source": {
     "type": "string",
     "description": "Client app identifier for verification. Download access and client-validation only."
    },
    "timeWindowSeconds": {
     "type": "integer",
     "description": "Time in seconds for license auto-expiration.",
     "format": "int32"
    },
    "volumeId": {
     "type": "string",
     "description": "Identifies the volume for which this entry applies."
    }
   }
  },
  "DownloadAccessRestriction": {
   "id": "DownloadAccessRestriction",
   "type": "object",
   "properties": {
    "deviceAllowed": {
     "type": "boolean",
     "description": "If restricted, whether access is granted for this (user, device, volume)."
    },
    "downloadsAcquired": {
     "type": "integer",
     "description": "If restricted, the number of content download licenses already acquired (including the requesting client, if licensed).",
     "format": "int32"
    },
    "justAcquired": {
     "type": "boolean",
     "description": "If deviceAllowed, whether access was just acquired with this request."
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "books#downloadAccessRestriction"
    },
    "maxDownloadDevices": {
     "type": "integer",
     "description": "If restricted, the maximum number of content download licenses for this volume.",
     "format": "int32"
    },
    "message": {
     "type": "string",
     "description": "Error/warning message."
    },
    "nonce": {
     "type": "string",
     "description": "Client nonce for verification. Download access and client-validation only."
    },
    "reasonCode": {
     "type": "string",
     "description": "Error/warning reason code. Additional codes may be added in the future. 0 OK 100 ACCESS_DENIED_PUBLISHER_LIMIT 101 ACCESS_DENIED_LIMIT 200 WARNING_USED_LAST_ACCESS"
    },
    "restricted": {
     "type": "boolean",
     "description": "Whether this volume has any download access restrictions."
    },
    "signature": {
     "type": "string",
     "description": "Response signature."
    },
    "source": {
     "type": "string",
     "description": "Client app identifier for verification. Download access and client-validation only."
    },
    "volumeId": {
     "type": "string",
     "description": "Identifies the volume for which this entry applies."
    }
   }
  },
  "DownloadAccesses": {
   "id": "DownloadAccesses",
   "type": "object",
   "properties": {
    "downloadAccessList": {
     "type": "array",
     "description": "A list of download access responses.",
     "items": {
      "$ref": "DownloadAccessRestriction"
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "books#downloadAccesses"
    }
   }
  },
  "Layersummaries": {
   "id": "Layersummaries",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of layer summary items.",
     "items": {
      "$ref": "Layersummary"
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "books#layersummaries"
    },
    "totalItems": {
     "type": "integer",
     "description": "The total number of layer summaries found.",
     "format": "int32"
    }
   }
  },
  "Layersummary": {
   "id": "Layersummary",
   "type": "object",
   "properties": {
    "annotationCount": {
     "type": "integer",
     "description": "The number of annotations for this layer.",
     "format": "int32"
    },
    "annotationTypes": {
     "type": "array",
     "description": "The list of annotation types contained for this layer.",
     "items": {
      "type": "string"
     }
    },
    "annotationsDataLink": {
     "type": "string",
     "description": "Link to get data for this annotation."
    },
    "annotationsLink": {
     "type": "string",
     "description": "The link to get the annotations for this layer."
    },
    "contentVersion": {
     "type": "string",
     "description": "The content version this resource is for."
    },
    "dataCount": {
     "type": "integer",
     "description": "The number of data items for this layer.",
     "format": "int32"
    },
    "id": {
     "type": "string",
     "description": "Unique id of this layer summary."
    },
    "kind": {
     "type": "string",
     "description": "Resource Type",
     "default": "books#layersummary"
    },
    "layerId": {
     "type": "string",
     "description": "The layer id for this summary."
    },
    "selfLink": {
     "type": "string",
     "description": "URL to this resource."
    },
    "updated": {
     "type": "string",
     "description": "Timestamp for the last time an item in this layer was updated. (RFC 3339 UTC date-time format).",
     "format": "date-time"
    },
    "volumeId": {
     "type": "string",
     "description": "The volume id this resource is for."
    }
   }
  },
  "ReadingPosition": {
   "id": "ReadingPosition",
   "type": "object",
   "properties": {
    "epubCfiPosition": {
     "type": "string",
     "description": "Position in an EPUB as a CFI."
    },
    "gbImagePosition": {
     "type": "string",
     "description": "Position in a volume for image-based content."
    },
    "gbTextPosition": {
     "type": "string",
     "description": "Position in a volume for text-based content."
    },
    "kind": {
     "type": "string",
     "description": "Resource type for a reading position.",
     "default": "books#readingPosition"
    },
    "pdfPosition": {
     "type": "string",
     "description": "Position in a PDF file."
    },
    "updated": {
     "type": "string",
     "description": "Timestamp when this reading position was last updated (formatted UTC timestamp with millisecond resolution).",
     "format": "date-time"
    },
    "volumeId": {
     "type": "string",
     "description": "Volume id associated with this reading position."
    }
   }
  },
  "RequestAccess": {
   "id": "RequestAccess",
   "type": "object",
   "properties": {
    "concurrentAccess": {
     "$ref": "ConcurrentAccessRestriction",
     "description": "A concurrent access response."
    },
    "downloadAccess": {
     "$ref": "DownloadAccessRestriction",
     "description": "A download access response."
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "books#requestAccess"
    }
   }
  },
  "Review": {
   "id": "Review",
   "type": "object",
   "properties": {
    "author": {
     "type": "object",
     "description": "Author of this review.",
     "properties": {
      "displayName": {
       "type": "string",
       "description": "Name of this person."
      }
     }
    },
    "content": {
     "type": "string",
     "description": "Review text."
    },
    "date": {
     "type": "string",
     "description": "Date of this review."
    },
    "fullTextUrl": {
     "type": "string",
     "description": "URL for the full review text, for reviews gathered from the web."
    },
    "kind": {
     "type": "string",
     "description": "Resource type for a review.",
     "default": "books#review"
    },
    "rating": {
     "type": "string",
     "description": "Star rating for this review. Possible values are ONE, TWO, THREE, FOUR, FIVE or NOT_RATED."
    },
    "source": {
     "type": "object",
     "description": "Information regarding the source of this review, when the review is not from a Google Books user.",
     "properties": {
      "description": {
       "type": "string",
       "description": "Name of the source."
      },
      "extraDescription": {
       "type": "string",
       "description": "Extra text about the source of the review."
      },
      "url": {
       "type": "string",
       "description": "URL of the source of the review."
      }
     }
    },
    "title": {
     "type": "string",
     "description": "Title for this review."
    },
    "type": {
     "type": "string",
     "description": "Source type for this review. Possible values are EDITORIAL, WEB_USER or GOOGLE_USER."
    },
    "volumeId": {
     "type": "string",
     "description": "Volume that this review is for."
    }
   }
  },
  "Volume": {
   "id": "Volume",
   "type": "object",
   "properties": {
    "accessInfo": {
     "type": "object",
     "description": "Any information about a volume related to reading or obtaining that volume text. This information can depend on country (books may be public domain in one country but not in another, e.g.).",
     "properties": {
      "accessViewStatus": {
       "type": "string",
       "description": "Combines the access and viewability of this volume into a single status field for this user. Values can be FULL_PURCHASED, FULL_PUBLIC_DOMAIN, SAMPLE or NONE. (In LITE projection.)"
      },
      "country": {
       "type": "string",
       "description": "The two-letter ISO_3166-1 country code for which this access information is valid. (In LITE projection.)"
      },
      "downloadAccess": {
       "$ref": "DownloadAccessRestriction",
       "description": "Information about a volume's download license access restrictions."
      },
      "embeddable": {
       "type": "boolean",
       "description": "Whether this volume can be embedded in a viewport using the Embedded Viewer API."
      },
      "epub": {
       "type": "object",
       "description": "Information about epub content. (In LITE projection.)",
       "properties": {
        "acsTokenLink": {
         "type": "string",
         "description": "URL to retrieve ACS token for epub download. (In LITE projection.)"
        },
        "downloadLink": {
         "type": "string",
         "description": "URL to download epub. (In LITE projection.)"
        },
        "isAvailable": {
         "type": "boolean",
         "description": "Is a flowing text epub available either as public domain or for purchase. (In LITE projection.)"
        }
       }
      },
      "pdf": {
       "type": "object",
       "description": "Information about pdf content. (In LITE projection.)",
       "properties": {
        "acsTokenLink": {
         "type": "string",
         "description": "URL to retrieve ACS token for pdf download. (In LITE projection.)"
        },
        "downloadLink": {
         "type": "string",
         "description": "URL to download pdf. (In LITE projection.)"
        },
        "isAvailable": {
         "type": "boolean",
         "description": "Is a scanned image pdf available either as public domain or for purchase. (In LITE projection.)"
        }
       }
      },
      "publicDomain": {
       "type": "boolean",
       "description": "Whether or not this book is public domain in the country listed above."
      },
      "textToSpeechPermission": {
       "type": "string",
       "description": "Whether text-to-speech is permitted for this volume. Values can be ALLOWED, ALLOWED_FOR_ACCESSIBILITY, or NOT_ALLOWED."
      },
      "viewOrderUrl": {
       "type": "string",
       "description": "For ordered but not yet processed orders, we give a URL that can be used to go to the appropriate Google Wallet page."
      },
      "viewability": {
       "type": "string",
       "description": "The read access of a volume. Possible values are PARTIAL, ALL_PAGES, NO_PAGES or UNKNOWN. This value depends on the country listed above. A value of PARTIAL means that the publisher has allowed some portion of the volume to be viewed publicly, without purchase. This can apply to eBooks as well as non-eBooks. Public domain books will always have a value of ALL_PAGES."
      },
      "webReaderLink": {
       "type": "string",
       "description": "URL to read this volume on the Google Books site. Link will not allow users to read non-viewable volumes."
      }
     }
    },
    "etag": {
     "type": "string",
     "description": "Opaque identifier for a specific version of a volume resource. (In LITE projection)"
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for a volume. (In LITE projection.)"
    },
    "kind": {
     "type": "string",
     "description": "Resource type for a volume. (In LITE projection.)",
     "default": "books#volume"
    },
    "saleInfo": {
     "type": "object",
     "description": "Any information about a volume related to the eBookstore and/or purchaseability. This information can depend on the country where the request originates from (i.e. books may not be for sale in certain countries).",
     "properties": {
      "buyLink": {
       "type": "string",
       "description": "URL to purchase this volume on the Google Books site. (In LITE projection)"
      },
      "country": {
       "type": "string",
       "description": "The two-letter ISO_3166-1 country code for which this sale information is valid. (In LITE projection.)"
      },
      "isEbook": {
       "type": "boolean",
       "description": "Whether or not this volume is an eBook (can be added to the My eBooks shelf)."
      },
      "listPrice": {
       "type": "object",
       "description": "Suggested retail price. (In LITE projection.)",
       "properties": {
        "amount": {
         "type": "number",
         "description": "Amount in the currency listed below. (In LITE projection.)",
         "format": "double"
        },
        "currencyCode": {
         "type": "string",
         "description": "An ISO 4217, three-letter currency code. (In LITE projection.)"
        }
       }
      },
      "onSaleDate": {
       "type": "string",
       "description": "The date on which this book is available for sale.",
       "format": "date-time"
      },
      "retailPrice": {
       "type": "object",
       "description": "The actual selling price of the book. This is the same as the suggested retail or list price unless there are offers or discounts on this volume. (In LITE projection.)",
       "properties": {
        "amount": {
         "type": "number",
         "description": "Amount in the currency listed below. (In LITE projection.)",
         "format": "double"
        },
        "currencyCode": {
         "type": "string",
         "description": "An ISO 4217, three-letter currency code. (In LITE projection.)"
        }
       }
      },
      "saleability": {
       "type": "string",
       "description": "Whether or not this book is available for sale or offered for free in the Google eBookstore for the country listed above. Possible values are FOR_SALE, FREE, NOT_FOR_SALE, or FOR_PREORDER."
      }
     }
    },
    "searchInfo": {
     "type": "object",
     "description": "Search result information related to this volume.",
     "properties": {
      "textSnippet": {
       "type": "string",
       "description": "A text snippet containing the search query."
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "URL to this resource. (In LITE projection.)"
    },
    "userInfo": {
     "type": "object",
     "description": "User specific information related to this volume. (e.g. page this user last read or whether they purchased this book)",
     "properties": {
      "isInMyBooks": {
       "type": "boolean",
       "description": "Whether or not this volume is currently in \"my books.\""
      },
      "isPreordered": {
       "type": "boolean",
       "description": "Whether or not this volume was pre-ordered by the authenticated user making the request. (In LITE projection.)"
      },
      "isPurchased": {
       "type": "boolean",
       "description": "Whether or not this volume was purchased by the authenticated user making the request. (In LITE projection.)"
      },
      "readingPosition": {
       "$ref": "ReadingPosition",
       "description": "The user's current reading position in the volume, if one is available. (In LITE projection.)"
      },
      "review": {
       "$ref": "Review",
       "description": "This user's review of this volume, if one exists."
      },
      "updated": {
       "type": "string",
       "description": "Timestamp when this volume was last modified by a user action, such as a reading position update, volume purchase or writing a review. (RFC 3339 UTC date-time format).",
       "format": "date-time"
      }
     }
    },
    "volumeInfo": {
     "type": "object",
     "description": "General volume information.",
     "properties": {
      "authors": {
       "type": "array",
       "description": "The names of the authors and/or editors for this volume. (In LITE projection)",
       "items": {
        "type": "string"
       }
      },
      "averageRating": {
       "type": "number",
       "description": "The mean review rating for this volume. (min = 1.0, max = 5.0)",
       "format": "double"
      },
      "canonicalVolumeLink": {
       "type": "string",
       "description": "Canonical URL for a volume. (In LITE projection.)"
      },
      "categories": {
       "type": "array",
       "description": "A list of subject categories, such as \"Fiction\", \"Suspense\", etc.",
       "items": {
        "type": "string"
       }
      },
      "contentVersion": {
       "type": "string",
       "description": "An identifier for the version of the volume content (text & images). (In LITE projection)"
      },
      "description": {
       "type": "string",
       "description": "A synopsis of the volume. The text of the description is formatted in HTML and includes simple formatting elements, such as b, i, and br tags. (In LITE projection.)"
      },
      "dimensions": {
       "type": "object",
       "description": "Physical dimensions of this volume.",
       "properties": {
        "height": {
         "type": "string",
         "description": "Height or length of this volume (in cm)."
        },
        "thickness": {
         "type": "string",
         "description": "Thickness of this volume (in cm)."
        },
        "width": {
         "type": "string",
         "description": "Width of this volume (in cm)."
        }
       }
      },
      "imageLinks": {
       "type": "object",
       "description": "A list of image links for all the sizes that are available. (In LITE projection.)",
       "properties": {
        "extraLarge": {
         "type": "string",
         "description": "Image link for extra large size (width of ~1280 pixels). (In LITE projection)"
        },
        "large": {
         "type": "string",
         "description": "Image link for large size (width of ~800 pixels). (In LITE projection)"
        },
        "medium": {
         "type": "string",
         "description": "Image link for medium size (width of ~575 pixels). (In LITE projection)"
        },
        "small": {
         "type": "string",
         "description": "Image link for small size (width of ~300 pixels). (In LITE projection)"
        },
        "smallThumbnail": {
         "type": "string",
         "description": "Image link for small thumbnail size (width of ~80 pixels). (In LITE projection)"
        },
        "thumbnail": {
         "type": "string",
         "description": "Image link for thumbnail size (width of ~128 pixels). (In LITE projection)"
        }
       }
      },
      "industryIdentifiers": {
       "type": "array",
       "description": "Industry standard identifiers for this volume.",
       "items": {
        "type": "object",
        "properties": {
         "identifier": {
          "type": "string",
          "description": "Industry specific volume identifier."
         },
         "type": {
          "type": "string",
          "description": "Identifier type. Possible values are ISBN_10, ISBN_13, ISSN and OTHER."
         }
        }
       }
      },
      "infoLink": {
       "type": "string",
       "description": "URL to view information about this volume on the Google Books site. (In LITE projection)"
      },
      "language": {
       "type": "string",
       "description": "Best language for this volume (based on content). It is the two-letter ISO 639-1 code such as 'fr', 'en', etc."
      },
      "mainCategory": {
       "type": "string",
       "description": "The main category to which this volume belongs. It will be the category from the categories list returned below that has the highest weight."
      },
      "pageCount": {
       "type": "integer",
       "description": "Total number of pages.",
       "format": "int32"
      },
      "previewLink": {
       "type": "string",
       "description": "URL to preview this volume on the Google Books site."
      },
      "printType": {
       "type": "string",
       "description": "Type of publication of this volume. Possible values are BOOK or MAGAZINE."
      },
      "publishedDate": {
       "type": "string",
       "description": "Date of publication. (In LITE projection.)"
      },
      "publisher": {
       "type": "string",
       "description": "Publisher of this volume. (In LITE projection.)"
      },
      "ratingsCount": {
       "type": "integer",
       "description": "The number of review ratings for this volume.",
       "format": "int32"
      },
      "subtitle": {
       "type": "string",
       "description": "Volume subtitle. (In LITE projection.)"
      },
      "title": {
       "type": "string",
       "description": "Volume title. (In LITE projection.)"
      }
     }
    }
   }
  },
  "Volumeannotation": {
   "id": "Volumeannotation",
   "type": "object",
   "properties": {
    "annotationDataId": {
     "type": "string",
     "description": "The annotation data id for this volume annotation."
    },
    "annotationDataLink": {
     "type": "string",
     "description": "Link to get data for this annotation."
    },
    "annotationType": {
     "type": "string",
     "description": "The type of annotation this is."
    },
    "contentRanges": {
     "type": "object",
     "description": "The content ranges to identify the selected text.",
     "properties": {
      "cfiRange": {
       "$ref": "BooksAnnotationsRange",
       "description": "Range in CFI format for this annotation for version above."
      },
      "contentVersion": {
       "type": "string",
       "description": "Content version applicable to ranges below."
      },
      "gbImageRange": {
       "$ref": "BooksAnnotationsRange",
       "description": "Range in GB image format for this annotation for version above."
      },
      "gbTextRange": {
       "$ref": "BooksAnnotationsRange",
       "description": "Range in GB text format for this annotation for version above."
      }
     }
    },
    "data": {
     "type": "string",
     "description": "Data for this annotation."
    },
    "deleted": {
     "type": "boolean",
     "description": "Indicates that this annotation is deleted."
    },
    "id": {
     "type": "string",
     "description": "Unique id of this volume annotation."
    },
    "kind": {
     "type": "string",
     "description": "Resource Type",
     "default": "books#volumeannotation"
    },
    "layerId": {
     "type": "string",
     "description": "The Layer this annotation is for."
    },
    "pageIds": {
     "type": "array",
     "description": "Pages the annotation spans.",
     "items": {
      "type": "string"
     }
    },
    "selectedText": {
     "type": "string",
     "description": "Excerpt from the volume."
    },
    "selfLink": {
     "type": "string",
     "description": "URL to this resource."
    },
    "updated": {
     "type": "string",
     "description": "Timestamp for the last time this anntoation was updated. (RFC 3339 UTC date-time format).",
     "format": "date-time"
    },
    "volumeId": {
     "type": "string",
     "description": "The Volume this annotation is for."
    }
   }
  },
  "Volumeannotations": {
   "id": "Volumeannotations",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of volume annotations.",
     "items": {
      "$ref": "Volumeannotation"
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type",
     "default": "books#volumeannotations"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token to pass in for pagination for the next page. This will not be present if this request does not have more results."
    },
    "totalItems": {
     "type": "integer",
     "description": "The total number of volume annotations found.",
     "format": "int32"
    }
   }
  },
  "Volumes": {
   "id": "Volumes",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "A list of volumes.",
     "items": {
      "$ref": "Volume"
     }
    },
    "kind": {
     "type": "string",
     "description": "Resource type.",
     "default": "books#volumes"
    },
    "totalItems": {
     "type": "integer",
     "description": "Total number of volumes found. This might be greater than the number of volumes returned in this response if results have been paginated.",
     "format": "int32"
    }
   }
  }
 },
 "resources": {
  "bookshelves": {
   "methods": {
    "get": {
     "id": "books.bookshelves.get",
     "path": "users/{userId}/bookshelves/{shelf}",
     "httpMethod": "GET",
     "description": "Retrieves metadata for a specific bookshelf for the specified user.",
     "parameters": {
      "shelf": {
       "type": "string",
       "description": "ID of bookshelf to retrieve.",
       "required": true,
       "location": "path"
      },
      "source": {
       "type": "string",
       "description": "String to identify the originator of this request.",
       "location": "query"
      },
      "userId": {
       "type": "string",
       "description": "ID of user for whom to retrieve bookshelves.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "userId",
      "shelf"
     ],
     "response": {
      "$ref": "Bookshelf"
     },
     "scopes": [
      "https://www.googleapis.com/auth/books"
     ]
    },
    "list": {
     "id": "books.bookshelves.list",
     "path": "users/{userId}/bookshelves",
     "httpMethod": "GET",
     "description": "Retrieves a list of public bookshelves for the specified user.",
     "parameters": {
      "source": {
       "type": "string",
       "description": "String to identify the originator of this request.",
       "location": "query"
      },
      "userId": {
       "type": "string",
       "description": "ID of user for whom to retrieve bookshelves.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "userId"
     ],
     "response": {
      "$ref": "Bookshelves"
     },
     "scopes": [
      "https://www.googleapis.com/auth/books"
     ]
    }
   },
   "resources": {
    "volumes": {
     "methods": {
      "list": {
       "id": "books.bookshelves.volumes.list",
       "path": "users/{userId}/bookshelves/{shelf}/volumes",
       "httpMethod": "GET",
       "description": "Retrieves volumes in a specific bookshelf for the specified user.",
       "parameters": {
        "maxResults": {
         "type": "integer",
         "description": "Maximum number of results to return",
         "format": "uint32",
         "minimum": "0",
         "location": "query"
        },
        "shelf": {
         "type": "string",
         "description": "ID of bookshelf to retrieve volumes.",
         "required": true,
         "location": "path"
        },
        "showPreorders": {
         "type": "boolean",
         "description": "Set to true to show pre-ordered books. Defaults to false.",
         "location": "query"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "startIndex": {
         "type": "integer",
         "description": "Index of the first element to return (starts at 0)",
         "format": "uint32",
         "minimum": "0",
         "location": "query"
        },
        "userId": {
         "type": "string",
         "description": "ID of user for whom to retrieve bookshelf volumes.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "userId",
        "shelf"
       ],
       "response": {
        "$ref": "Volumes"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      }
     }
    }
   }
  },
  "layers": {
   "methods": {
    "get": {
     "id": "books.layers.get",
     "path": "volumes/{volumeId}/layersummary/{summaryId}",
     "httpMethod": "GET",
     "description": "Gets the layer summary for a volume.",
     "parameters": {
      "contentVersion": {
       "type": "string",
       "description": "The content version for the requested volume.",
       "location": "query"
      },
      "source": {
       "type": "string",
       "description": "String to identify the originator of this request.",
       "location": "query"
      },
      "summaryId": {
       "type": "string",
       "description": "The ID for the layer to get the summary for.",
       "required": true,
       "location": "path"
      },
      "volumeId": {
       "type": "string",
       "description": "The volume to retrieve layers for.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "volumeId",
      "summaryId"
     ],
     "response": {
      "$ref": "Layersummary"
     },
     "scopes": [
      "https://www.googleapis.com/auth/books"
     ]
    },
    "list": {
     "id": "books.layers.list",
     "path": "volumes/{volumeId}/layersummary",
     "httpMethod": "GET",
     "description": "List the layer summaries for a volume.",
     "parameters": {
      "contentVersion": {
       "type": "string",
       "description": "The content version for the requested volume.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "format": "uint32",
       "minimum": "0",
       "maximum": "200",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "The value of the nextToken from the previous page.",
       "location": "query"
      },
      "source": {
       "type": "string",
       "description": "String to identify the originator of this request.",
       "location": "query"
      },
      "volumeId": {
       "type": "string",
       "description": "The volume to retrieve layers for.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "volumeId"
     ],
     "response": {
      "$ref": "Layersummaries"
     },
     "scopes": [
      "https://www.googleapis.com/auth/books"
     ]
    }
   },
   "resources": {
    "annotationData": {
     "methods": {
      "get": {
       "id": "books.layers.annotationData.get",
       "path": "volumes/{volumeId}/layers/{layerId}/data/{annotationDataId}",
       "httpMethod": "GET",
       "description": "Gets the annotation data.",
       "parameters": {
        "annotationDataId": {
         "type": "string",
         "description": "The ID of the annotation data to retrieve.",
         "required": true,
         "location": "path"
        },
        "contentVersion": {
         "type": "string",
         "description": "The content version for the volume you are trying to retrieve.",
         "required": true,
         "location": "query"
        },
        "h": {
         "type": "integer",
         "description": "The requested pixel height for any images. If height is provided width must also be provided.",
         "format": "int32",
         "location": "query"
        },
        "layerId": {
         "type": "string",
         "description": "The ID for the layer to get the annotations.",
         "required": true,
         "location": "path"
        },
        "locale": {
         "type": "string",
         "description": "The locale information for the data. ISO-639-1 language and ISO-3166-1 country code. Ex: 'en_US'.",
         "location": "query"
        },
        "scale": {
         "type": "integer",
         "description": "The requested scale for the image.",
         "format": "int32",
         "minimum": "0",
         "location": "query"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "The volume to retrieve annotations for.",
         "required": true,
         "location": "path"
        },
        "w": {
         "type": "integer",
         "description": "The requested pixel width for any images. If width is provided height must also be provided.",
         "format": "int32",
         "location": "query"
        }
       },
       "parameterOrder": [
        "volumeId",
        "layerId",
        "annotationDataId",
        "contentVersion"
       ],
       "response": {
        "$ref": "Annotationdata"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "list": {
       "id": "books.layers.annotationData.list",
       "path": "volumes/{volumeId}/layers/{layerId}/data",
       "httpMethod": "GET",
       "description": "Gets the annotation data for a volume and layer.",
       "parameters": {
        "annotationDataId": {
         "type": "string",
         "description": "The list of Annotation Data Ids to retrieve. Pagination is ignored if this is set.",
         "repeated": true,
         "location": "query"
        },
        "contentVersion": {
         "type": "string",
         "description": "The content version for the requested volume.",
         "required": true,
         "location": "query"
        },
        "h": {
         "type": "integer",
         "description": "The requested pixel height for any images. If height is provided width must also be provided.",
         "format": "int32",
         "location": "query"
        },
        "layerId": {
         "type": "string",
         "description": "The ID for the layer to get the annotation data.",
         "required": true,
         "location": "path"
        },
        "locale": {
         "type": "string",
         "description": "The locale information for the data. ISO-639-1 language and ISO-3166-1 country code. Ex: 'en_US'.",
         "location": "query"
        },
        "maxResults": {
         "type": "integer",
         "description": "Maximum number of results to return",
         "format": "uint32",
         "minimum": "0",
         "maximum": "200",
         "location": "query"
        },
        "pageToken": {
         "type": "string",
         "description": "The value of the nextToken from the previous page.",
         "location": "query"
        },
        "scale": {
         "type": "integer",
         "description": "The requested scale for the image.",
         "format": "int32",
         "minimum": "0",
         "location": "query"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "updatedMax": {
         "type": "string",
         "description": "RFC 3339 timestamp to restrict to items updated prior to this timestamp (exclusive).",
         "location": "query"
        },
        "updatedMin": {
         "type": "string",
         "description": "RFC 3339 timestamp to restrict to items updated since this timestamp (inclusive).",
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "The volume to retrieve annotation data for.",
         "required": true,
         "location": "path"
        },
        "w": {
         "type": "integer",
         "description": "The requested pixel width for any images. If width is provided height must also be provided.",
         "format": "int32",
         "location": "query"
        }
       },
       "parameterOrder": [
        "volumeId",
        "layerId",
        "contentVersion"
       ],
       "response": {
        "$ref": "Annotationsdata"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      }
     }
    },
    "volumeAnnotations": {
     "methods": {
      "get": {
       "id": "books.layers.volumeAnnotations.get",
       "path": "volumes/{volumeId}/layers/{layerId}/annotations/{annotationId}",
       "httpMethod": "GET",
       "description": "Gets the volume annotation.",
       "parameters": {
        "annotationId": {
         "type": "string",
         "description": "The ID of the volume annotation to retrieve.",
         "required": true,
         "location": "path"
        },
        "layerId": {
         "type": "string",
         "description": "The ID for the layer to get the annotations.",
         "required": true,
         "location": "path"
        },
        "locale": {
         "type": "string",
         "description": "The locale information for the data. ISO-639-1 language and ISO-3166-1 country code. Ex: 'en_US'.",
         "location": "query"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "The volume to retrieve annotations for.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "volumeId",
        "layerId",
        "annotationId"
       ],
       "response": {
        "$ref": "Volumeannotation"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "list": {
       "id": "books.layers.volumeAnnotations.list",
       "path": "volumes/{volumeId}/layers/{layerId}",
       "httpMethod": "GET",
       "description": "Gets the volume annotations for a volume and layer.",
       "parameters": {
        "contentVersion": {
         "type": "string",
         "description": "The content version for the requested volume.",
         "required": true,
         "location": "query"
        },
        "endOffset": {
         "type": "string",
         "description": "The end offset to end retrieving data from.",
         "location": "query"
        },
        "endPosition": {
         "type": "string",
         "description": "The end position to end retrieving data from.",
         "location": "query"
        },
        "layerId": {
         "type": "string",
         "description": "The ID for the layer to get the annotations.",
         "required": true,
         "location": "path"
        },
        "locale": {
         "type": "string",
         "description": "The locale information for the data. ISO-639-1 language and ISO-3166-1 country code. Ex: 'en_US'.",
         "location": "query"
        },
        "maxResults": {
         "type": "integer",
         "description": "Maximum number of results to return",
         "format": "uint32",
         "minimum": "0",
         "maximum": "200",
         "location": "query"
        },
        "pageToken": {
         "type": "string",
         "description": "The value of the nextToken from the previous page.",
         "location": "query"
        },
        "showDeleted": {
         "type": "boolean",
         "description": "Set to true to return deleted annotations. updatedMin must be in the request to use this. Defaults to false.",
         "location": "query"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "startOffset": {
         "type": "string",
         "description": "The start offset to start retrieving data from.",
         "location": "query"
        },
        "startPosition": {
         "type": "string",
         "description": "The start position to start retrieving data from.",
         "location": "query"
        },
        "updatedMax": {
         "type": "string",
         "description": "RFC 3339 timestamp to restrict to items updated prior to this timestamp (exclusive).",
         "location": "query"
        },
        "updatedMin": {
         "type": "string",
         "description": "RFC 3339 timestamp to restrict to items updated since this timestamp (inclusive).",
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "The volume to retrieve annotations for.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "volumeId",
        "layerId",
        "contentVersion"
       ],
       "response": {
        "$ref": "Volumeannotations"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      }
     }
    }
   }
  },
  "myconfig": {
   "methods": {
    "releaseDownloadAccess": {
     "id": "books.myconfig.releaseDownloadAccess",
     "path": "myconfig/releaseDownloadAccess",
     "httpMethod": "POST",
     "description": "Release downloaded content access restriction.",
     "parameters": {
      "cpksver": {
       "type": "string",
       "description": "The device/version ID from which to release the restriction.",
       "required": true,
       "location": "query"
      },
      "locale": {
       "type": "string",
       "description": "ISO-639-1, ISO-3166-1 codes for message localization, i.e. en_US.",
       "location": "query"
      },
      "source": {
       "type": "string",
       "description": "String to identify the originator of this request.",
       "location": "query"
      },
      "volumeIds": {
       "type": "string",
       "description": "The volume(s) to release restrictions for.",
       "required": true,
       "repeated": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "volumeIds",
      "cpksver"
     ],
     "response": {
      "$ref": "DownloadAccesses"
     },
     "scopes": [
      "https://www.googleapis.com/auth/books"
     ]
    },
    "requestAccess": {
     "id": "books.myconfig.requestAccess",
     "path": "myconfig/requestAccess",
     "httpMethod": "POST",
     "description": "Request concurrent and download access restrictions.",
     "parameters": {
      "cpksver": {
       "type": "string",
       "description": "The device/version ID from which to request the restrictions.",
       "required": true,
       "location": "query"
      },
      "locale": {
       "type": "string",
       "description": "ISO-639-1, ISO-3166-1 codes for message localization, i.e. en_US.",
       "location": "query"
      },
      "nonce": {
       "type": "string",
       "description": "The client nonce value.",
       "required": true,
       "location": "query"
      },
      "source": {
       "type": "string",
       "description": "String to identify the originator of this request.",
       "required": true,
       "location": "query"
      },
      "volumeId": {
       "type": "string",
       "description": "The volume to request concurrent/download restrictions for.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "source",
      "volumeId",
      "nonce",
      "cpksver"
     ],
     "response": {
      "$ref": "RequestAccess"
     },
     "scopes": [
      "https://www.googleapis.com/auth/books"
     ]
    },
    "syncVolumeLicenses": {
     "id": "books.myconfig.syncVolumeLicenses",
     "path": "myconfig/syncVolumeLicenses",
     "httpMethod": "POST",
     "description": "Request downloaded content access for specified volumes on the My eBooks shelf.",
     "parameters": {
      "cpksver": {
       "type": "string",
       "description": "The device/version ID from which to release the restriction.",
       "required": true,
       "location": "query"
      },
      "locale": {
       "type": "string",
       "description": "ISO-639-1, ISO-3166-1 codes for message localization, i.e. en_US.",
       "location": "query"
      },
      "nonce": {
       "type": "string",
       "description": "The client nonce value.",
       "required": true,
       "location": "query"
      },
      "showPreorders": {
       "type": "boolean",
       "description": "Set to true to show pre-ordered books. Defaults to false.",
       "location": "query"
      },
      "source": {
       "type": "string",
       "description": "String to identify the originator of this request.",
       "required": true,
       "location": "query"
      },
      "volumeIds": {
       "type": "string",
       "description": "The volume(s) to request download restrictions for.",
       "repeated": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "source",
      "nonce",
      "cpksver"
     ],
     "response": {
      "$ref": "Volumes"
     },
     "scopes": [
      "https://www.googleapis.com/auth/books"
     ]
    }
   }
  },
  "mylibrary": {
   "resources": {
    "annotations": {
     "methods": {
      "delete": {
       "id": "books.mylibrary.annotations.delete",
       "path": "mylibrary/annotations/{annotationId}",
       "httpMethod": "DELETE",
       "description": "Deletes an annotation.",
       "parameters": {
        "annotationId": {
         "type": "string",
         "description": "The ID for the annotation to delete.",
         "required": true,
         "location": "path"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "annotationId"
       ],
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "get": {
       "id": "books.mylibrary.annotations.get",
       "path": "mylibrary/annotations/{annotationId}",
       "httpMethod": "GET",
       "description": "Gets an annotation by its ID.",
       "parameters": {
        "annotationId": {
         "type": "string",
         "description": "The ID for the annotation to retrieve.",
         "required": true,
         "location": "path"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "annotationId"
       ],
       "response": {
        "$ref": "Annotation"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "insert": {
       "id": "books.mylibrary.annotations.insert",
       "path": "mylibrary/annotations",
       "httpMethod": "POST",
       "description": "Inserts a new annotation.",
       "parameters": {
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        }
       },
       "request": {
        "$ref": "Annotation"
       },
       "response": {
        "$ref": "Annotation"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "list": {
       "id": "books.mylibrary.annotations.list",
       "path": "mylibrary/annotations",
       "httpMethod": "GET",
       "description": "Retrieves a list of annotations, possibly filtered.",
       "parameters": {
        "contentVersion": {
         "type": "string",
         "description": "The content version for the requested volume.",
         "location": "query"
        },
        "layerId": {
         "type": "string",
         "description": "The layer ID to limit annotation by.",
         "location": "query"
        },
        "maxResults": {
         "type": "integer",
         "description": "Maximum number of results to return",
         "format": "uint32",
         "minimum": "0",
         "maximum": "40",
         "location": "query"
        },
        "pageIds": {
         "type": "string",
         "description": "The page ID(s) for the volume that is being queried.",
         "repeated": true,
         "location": "query"
        },
        "pageToken": {
         "type": "string",
         "description": "The value of the nextToken from the previous page.",
         "location": "query"
        },
        "showDeleted": {
         "type": "boolean",
         "description": "Set to true to return deleted annotations. updatedMin must be in the request to use this. Defaults to false.",
         "location": "query"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "updatedMax": {
         "type": "string",
         "description": "RFC 3339 timestamp to restrict to items updated prior to this timestamp (exclusive).",
         "location": "query"
        },
        "updatedMin": {
         "type": "string",
         "description": "RFC 3339 timestamp to restrict to items updated since this timestamp (inclusive).",
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "The volume to restrict annotations to.",
         "location": "query"
        }
       },
       "response": {
        "$ref": "Annotations"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "update": {
       "id": "books.mylibrary.annotations.update",
       "path": "mylibrary/annotations/{annotationId}",
       "httpMethod": "PUT",
       "description": "Updates an existing annotation.",
       "parameters": {
        "annotationId": {
         "type": "string",
         "description": "The ID for the annotation to update.",
         "required": true,
         "location": "path"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "annotationId"
       ],
       "request": {
        "$ref": "Annotation"
       },
       "response": {
        "$ref": "Annotation"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      }
     }
    },
    "bookshelves": {
     "methods": {
      "addVolume": {
       "id": "books.mylibrary.bookshelves.addVolume",
       "path": "mylibrary/bookshelves/{shelf}/addVolume",
       "httpMethod": "POST",
       "description": "Adds a volume to a bookshelf.",
       "parameters": {
        "shelf": {
         "type": "string",
         "description": "ID of bookshelf to which to add a volume.",
         "required": true,
         "location": "path"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "ID of volume to add.",
         "required": true,
         "location": "query"
        }
       },
       "parameterOrder": [
        "shelf",
        "volumeId"
       ],
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "clearVolumes": {
       "id": "books.mylibrary.bookshelves.clearVolumes",
       "path": "mylibrary/bookshelves/{shelf}/clearVolumes",
       "httpMethod": "POST",
       "description": "Clears all volumes from a bookshelf.",
       "parameters": {
        "shelf": {
         "type": "string",
         "description": "ID of bookshelf from which to remove a volume.",
         "required": true,
         "location": "path"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "shelf"
       ],
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "get": {
       "id": "books.mylibrary.bookshelves.get",
       "path": "mylibrary/bookshelves/{shelf}",
       "httpMethod": "GET",
       "description": "Retrieves metadata for a specific bookshelf belonging to the authenticated user.",
       "parameters": {
        "shelf": {
         "type": "string",
         "description": "ID of bookshelf to retrieve.",
         "required": true,
         "location": "path"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        }
       },
       "parameterOrder": [
        "shelf"
       ],
       "response": {
        "$ref": "Bookshelf"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "list": {
       "id": "books.mylibrary.bookshelves.list",
       "path": "mylibrary/bookshelves",
       "httpMethod": "GET",
       "description": "Retrieves a list of bookshelves belonging to the authenticated user.",
       "parameters": {
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        }
       },
       "response": {
        "$ref": "Bookshelves"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "moveVolume": {
       "id": "books.mylibrary.bookshelves.moveVolume",
       "path": "mylibrary/bookshelves/{shelf}/moveVolume",
       "httpMethod": "POST",
       "description": "Moves a volume within a bookshelf.",
       "parameters": {
        "shelf": {
         "type": "string",
         "description": "ID of bookshelf with the volume.",
         "required": true,
         "location": "path"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "ID of volume to move.",
         "required": true,
         "location": "query"
        },
        "volumePosition": {
         "type": "integer",
         "description": "Position on shelf to move the item (0 puts the item before the current first item, 1 puts it between the first and the second and so on.)",
         "required": true,
         "format": "int32",
         "location": "query"
        }
       },
       "parameterOrder": [
        "shelf",
        "volumeId",
        "volumePosition"
       ],
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "removeVolume": {
       "id": "books.mylibrary.bookshelves.removeVolume",
       "path": "mylibrary/bookshelves/{shelf}/removeVolume",
       "httpMethod": "POST",
       "description": "Removes a volume from a bookshelf.",
       "parameters": {
        "shelf": {
         "type": "string",
         "description": "ID of bookshelf from which to remove a volume.",
         "required": true,
         "location": "path"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "ID of volume to remove.",
         "required": true,
         "location": "query"
        }
       },
       "parameterOrder": [
        "shelf",
        "volumeId"
       ],
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      }
     },
     "resources": {
      "volumes": {
       "methods": {
        "list": {
         "id": "books.mylibrary.bookshelves.volumes.list",
         "path": "mylibrary/bookshelves/{shelf}/volumes",
         "httpMethod": "GET",
         "description": "Gets volume information for volumes on a bookshelf.",
         "parameters": {
          "country": {
           "type": "string",
           "description": "ISO-3166-1 code to override the IP-based location.",
           "location": "query"
          },
          "maxResults": {
           "type": "integer",
           "description": "Maximum number of results to return",
           "format": "uint32",
           "minimum": "0",
           "location": "query"
          },
          "projection": {
           "type": "string",
           "description": "Restrict information returned to a set of selected fields.",
           "enum": [
            "full",
            "lite"
           ],
           "enumDescriptions": [
            "Includes all volume data.",
            "Includes a subset of fields in volumeInfo and accessInfo."
           ],
           "location": "query"
          },
          "q": {
           "type": "string",
           "description": "Full-text search query string in this bookshelf.",
           "location": "query"
          },
          "shelf": {
           "type": "string",
           "description": "The bookshelf ID or name retrieve volumes for.",
           "required": true,
           "location": "path"
          },
          "showPreorders": {
           "type": "boolean",
           "description": "Set to true to show pre-ordered books. Defaults to false.",
           "location": "query"
          },
          "source": {
           "type": "string",
           "description": "String to identify the originator of this request.",
           "location": "query"
          },
          "startIndex": {
           "type": "integer",
           "description": "Index of the first element to return (starts at 0)",
           "format": "uint32",
           "minimum": "0",
           "location": "query"
          }
         },
         "parameterOrder": [
          "shelf"
         ],
         "response": {
          "$ref": "Volumes"
         },
         "scopes": [
          "https://www.googleapis.com/auth/books"
         ]
        }
       }
      }
     }
    },
    "readingpositions": {
     "methods": {
      "get": {
       "id": "books.mylibrary.readingpositions.get",
       "path": "mylibrary/readingpositions/{volumeId}",
       "httpMethod": "GET",
       "description": "Retrieves my reading position information for a volume.",
       "parameters": {
        "contentVersion": {
         "type": "string",
         "description": "Volume content version for which this reading position is requested.",
         "location": "query"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "ID of volume for which to retrieve a reading position.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "volumeId"
       ],
       "response": {
        "$ref": "ReadingPosition"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      },
      "setPosition": {
       "id": "books.mylibrary.readingpositions.setPosition",
       "path": "mylibrary/readingpositions/{volumeId}/setPosition",
       "httpMethod": "POST",
       "description": "Sets my reading position information for a volume.",
       "parameters": {
        "action": {
         "type": "string",
         "description": "Action that caused this reading position to be set.",
         "enum": [
          "bookmark",
          "chapter",
          "next-page",
          "prev-page",
          "scroll",
          "search"
         ],
         "enumDescriptions": [
          "User chose bookmark within volume.",
          "User selected chapter from list.",
          "Next page event.",
          "Previous page event.",
          "User navigated to page.",
          "User chose search results within volume."
         ],
         "location": "query"
        },
        "contentVersion": {
         "type": "string",
         "description": "Volume content version for which this reading position applies.",
         "location": "query"
        },
        "position": {
         "type": "string",
         "description": "Position string for the new volume reading position.",
         "required": true,
         "location": "query"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "timestamp": {
         "type": "string",
         "description": "RFC 3339 UTC format timestamp associated with this reading position.",
         "required": true,
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "ID of volume for which to update the reading position.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "volumeId",
        "timestamp",
        "position"
       ],
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      }
     }
    }
   }
  },
  "volumes": {
   "methods": {
    "get": {
     "id": "books.volumes.get",
     "path": "volumes/{volumeId}",
     "httpMethod": "GET",
     "description": "Gets volume information for a single volume.",
     "parameters": {
      "country": {
       "type": "string",
       "description": "ISO-3166-1 code to override the IP-based location.",
       "location": "query"
      },
      "partner": {
       "type": "string",
       "description": "Brand results for partner ID.",
       "location": "query"
      },
      "projection": {
       "type": "string",
       "description": "Restrict information returned to a set of selected fields.",
       "enum": [
        "full",
        "lite"
       ],
       "enumDescriptions": [
        "Includes all volume data.",
        "Includes a subset of fields in volumeInfo and accessInfo."
       ],
       "location": "query"
      },
      "source": {
       "type": "string",
       "description": "String to identify the originator of this request.",
       "location": "query"
      },
      "volumeId": {
       "type": "string",
       "description": "ID of volume to retrieve.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "volumeId"
     ],
     "response": {
      "$ref": "Volume"
     },
     "scopes": [
      "https://www.googleapis.com/auth/books"
     ]
    },
    "list": {
     "id": "books.volumes.list",
     "path": "volumes",
     "httpMethod": "GET",
     "description": "Performs a book search.",
     "parameters": {
      "download": {
       "type": "string",
       "description": "Restrict to volumes by download availability.",
       "enum": [
        "epub"
       ],
       "enumDescriptions": [
        "All volumes with epub."
       ],
       "location": "query"
      },
      "filter": {
       "type": "string",
       "description": "Filter search results.",
       "enum": [
        "ebooks",
        "free-ebooks",
        "full",
        "paid-ebooks",
        "partial"
       ],
       "enumDescriptions": [
        "All Google eBooks.",
        "Google eBook with full volume text viewability.",
        "Public can view entire volume text.",
        "Google eBook with a price.",
        "Public able to see parts of text."
       ],
       "location": "query"
      },
      "langRestrict": {
       "type": "string",
       "description": "Restrict results to books with this language code.",
       "location": "query"
      },
      "libraryRestrict": {
       "type": "string",
       "description": "Restrict search to this user's library.",
       "enum": [
        "my-library",
        "no-restrict"
       ],
       "enumDescriptions": [
        "Restrict to the user's library, any shelf.",
        "Do not restrict based on user's library."
       ],
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return.",
       "format": "uint32",
       "minimum": "0",
       "maximum": "40",
       "location": "query"
      },
      "orderBy": {
       "type": "string",
       "description": "Sort search results.",
       "enum": [
        "newest",
        "relevance"
       ],
       "enumDescriptions": [
        "Most recently published.",
        "Relevance to search terms."
       ],
       "location": "query"
      },
      "partner": {
       "type": "string",
       "description": "Restrict and brand results for partner ID.",
       "location": "query"
      },
      "printType": {
       "type": "string",
       "description": "Restrict to books or magazines.",
       "enum": [
        "all",
        "books",
        "magazines"
       ],
       "enumDescriptions": [
        "All volume content types.",
        "Just books.",
        "Just magazines."
       ],
       "location": "query"
      },
      "projection": {
       "type": "string",
       "description": "Restrict information returned to a set of selected fields.",
       "enum": [
        "full",
        "lite"
       ],
       "enumDescriptions": [
        "Includes all volume data.",
        "Includes a subset of fields in volumeInfo and accessInfo."
       ],
       "location": "query"
      },
      "q": {
       "type": "string",
       "description": "Full-text search query string.",
       "required": true,
       "location": "query"
      },
      "showPreorders": {
       "type": "boolean",
       "description": "Set to true to show books available for preorder. Defaults to false.",
       "location": "query"
      },
      "source": {
       "type": "string",
       "description": "String to identify the originator of this request.",
       "location": "query"
      },
      "startIndex": {
       "type": "integer",
       "description": "Index of the first result to return (starts at 0)",
       "format": "uint32",
       "minimum": "0",
       "location": "query"
      }
     },
     "parameterOrder": [
      "q"
     ],
     "response": {
      "$ref": "Volumes"
     },
     "scopes": [
      "https://www.googleapis.com/auth/books"
     ]
    }
   },
   "resources": {
    "associated": {
     "methods": {
      "list": {
       "id": "books.volumes.associated.list",
       "path": "volumes/{volumeId}/associated",
       "httpMethod": "GET",
       "description": "Return a list of associated books.",
       "parameters": {
        "association": {
         "type": "string",
         "description": "Association type.",
         "enum": [
          "end-of-sample",
          "end-of-volume"
         ],
         "enumDescriptions": [
          "Recommendations for display end-of-sample.",
          "Recommendations for display end-of-volume."
         ],
         "location": "query"
        },
        "locale": {
         "type": "string",
         "description": "ISO-639-1 language and ISO-3166-1 country code. Ex: 'en_US'. Used for generating recommendations.",
         "location": "query"
        },
        "source": {
         "type": "string",
         "description": "String to identify the originator of this request.",
         "location": "query"
        },
        "volumeId": {
         "type": "string",
         "description": "ID of the source volume.",
         "required": true,
         "location": "path"
        }
       },
       "parameterOrder": [
        "volumeId"
       ],
       "response": {
        "$ref": "Volumes"
       },
       "scopes": [
        "https://www.googleapis.com/auth/books"
       ]
      }
     }
    }
   }
  }
 }
}
