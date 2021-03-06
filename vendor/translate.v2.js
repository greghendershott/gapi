{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "translate:v2",
 "name": "translate",
 "version": "v2",
 "revision": "20120112",
 "title": "Translate API",
 "description": "Lets you translate text from one language to another",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/translate-16.png",
  "x32": "http://www.google.com/images/icons/product/translate-32.png"
 },
 "documentationLink": "http://code.google.com/apis/language/translate/v2/using_rest.html",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/language/translate/",
 "basePath": "/language/translate/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "language/translate/",
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
 "features": [
  "dataWrapper"
 ],
 "schemas": {
  "DetectionsListResponse": {
   "id": "DetectionsListResponse",
   "type": "object",
   "properties": {
    "detections": {
     "type": "array",
     "description": "A detections contains detection results of several text",
     "items": {
      "$ref": "DetectionsResource"
     }
    }
   }
  },
  "DetectionsResource": {
   "id": "DetectionsResource",
   "type": "array",
   "description": "An array of languages which we detect for the given text The most likely language list first.",
   "items": {
    "type": "object",
    "properties": {
     "confidence": {
      "type": "number",
      "description": "The confidence of the detection resul of this language.",
      "format": "float"
     },
     "isReliable": {
      "type": "boolean",
      "description": "A boolean to indicate is the language detection result reliable."
     },
     "language": {
      "type": "string",
      "description": "The language we detect"
     }
    }
   }
  },
  "LanguagesListResponse": {
   "id": "LanguagesListResponse",
   "type": "object",
   "properties": {
    "languages": {
     "type": "array",
     "description": "List of source/target languages supported by the translation API. If target parameter is unspecified, the list is sorted by the ASCII code point order of the language code. If target parameter is specified, the list is sorted by the collation order of the language name in the target language.",
     "items": {
      "$ref": "LanguagesResource"
     }
    }
   }
  },
  "LanguagesResource": {
   "id": "LanguagesResource",
   "type": "object",
   "properties": {
    "language": {
     "type": "string",
     "description": "The language code."
    },
    "name": {
     "type": "string",
     "description": "The localized name of the language if target parameter is given."
    }
   }
  },
  "TranslationsListResponse": {
   "id": "TranslationsListResponse",
   "type": "object",
   "properties": {
    "translations": {
     "type": "array",
     "description": "Translations contains list of translation results of given text",
     "items": {
      "$ref": "TranslationsResource"
     }
    }
   }
  },
  "TranslationsResource": {
   "id": "TranslationsResource",
   "type": "object",
   "properties": {
    "detectedSourceLanguage": {
     "type": "string",
     "description": "Detected source language if source parameter is unspecified."
    },
    "translatedText": {
     "type": "string",
     "description": "The translation."
    }
   }
  }
 },
 "resources": {
  "detections": {
   "methods": {
    "list": {
     "id": "language.detections.list",
     "path": "v2/detect",
     "httpMethod": "GET",
     "description": "Detect the language of text.",
     "parameters": {
      "q": {
       "type": "string",
       "description": "The text to detect",
       "required": true,
       "repeated": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "q"
     ],
     "response": {
      "$ref": "DetectionsListResponse"
     }
    }
   }
  },
  "languages": {
   "methods": {
    "list": {
     "id": "language.languages.list",
     "path": "v2/languages",
     "httpMethod": "GET",
     "description": "List the source/target languages supported by the API",
     "parameters": {
      "target": {
       "type": "string",
       "description": "the language and collation in which the localized results should be returned",
       "location": "query"
      }
     },
     "response": {
      "$ref": "LanguagesListResponse"
     }
    }
   }
  },
  "translations": {
   "methods": {
    "list": {
     "id": "language.translations.list",
     "path": "v2",
     "httpMethod": "GET",
     "description": "Returns text translations from one language to another.",
     "parameters": {
      "cid": {
       "type": "string",
       "description": "The customization id for translate",
       "repeated": true,
       "location": "query"
      },
      "format": {
       "type": "string",
       "description": "The format of the text",
       "enum": [
        "html",
        "text"
       ],
       "enumDescriptions": [
        "Specifies the input is in HTML",
        "Specifies the input is in plain textual format"
       ],
       "location": "query"
      },
      "q": {
       "type": "string",
       "description": "The text to translate",
       "required": true,
       "repeated": true,
       "location": "query"
      },
      "source": {
       "type": "string",
       "description": "The source language of the text",
       "location": "query"
      },
      "target": {
       "type": "string",
       "description": "The target language into which the text should be translated",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "q",
      "target"
     ],
     "response": {
      "$ref": "TranslationsListResponse"
     }
    }
   }
  }
 }
}
