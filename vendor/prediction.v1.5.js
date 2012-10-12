{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "prediction:v1.5",
 "name": "prediction",
 "version": "v1.5",
 "revision": "20120724",
 "title": "Prediction API",
 "description": "Lets you access a cloud hosted machine learning service that makes it easy to build smart apps",
 "icons": {
  "x16": "http://www.google.com/images/icons/feature/predictionapi-16.png",
  "x32": "http://www.google.com/images/icons/feature/predictionapi-32.png"
 },
 "documentationLink": "https://developers.google.com/prediction/docs/developer-guide",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/prediction/v1.5/",
 "basePath": "/prediction/v1.5/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "prediction/v1.5/",
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
    "https://www.googleapis.com/auth/devstorage.read_only": {
     "description": "View your data in Google Cloud Storage"
    },
    "https://www.googleapis.com/auth/prediction": {
     "description": "Manage your data in the Google Prediction API"
    }
   }
  }
 },
 "schemas": {
  "Analyze": {
   "id": "Analyze",
   "type": "object",
   "properties": {
    "dataDescription": {
     "type": "object",
     "description": "Description of the data the model was trained on.",
     "properties": {
      "features": {
       "type": "array",
       "description": "Description of the input features in the data set.",
       "items": {
        "type": "object",
        "properties": {
         "categorical": {
          "type": "object",
          "description": "Description of the categorical values of this feature.",
          "properties": {
           "count": {
            "type": "string",
            "description": "Number of categorical values for this feature in the data.",
            "format": "int64"
           },
           "values": {
            "type": "array",
            "description": "List of all the categories for this feature in the data set.",
            "items": {
             "type": "object",
             "properties": {
              "count": {
               "type": "string",
               "description": "Number of times this feature had this value.",
               "format": "int64"
              },
              "value": {
               "type": "string",
               "description": "The category name."
              }
             }
            }
           }
          }
         },
         "index": {
          "type": "string",
          "description": "The feature index.",
          "format": "int64"
         },
         "numeric": {
          "type": "object",
          "description": "Description of the numeric values of this feature.",
          "properties": {
           "count": {
            "type": "string",
            "description": "Number of numeric values for this feature in the data set.",
            "format": "int64"
           },
           "mean": {
            "type": "number",
            "description": "Mean of the numeric values of this feature in the data set.",
            "format": "double"
           },
           "variance": {
            "type": "number",
            "description": "Variance of the numeric values of this feature in the data set.",
            "format": "double"
           }
          }
         },
         "text": {
          "type": "object",
          "description": "Description of multiple-word text values of this feature.",
          "properties": {
           "count": {
            "type": "string",
            "description": "Number of multiple-word text values for this feature.",
            "format": "int64"
           }
          }
         }
        }
       }
      },
      "outputFeature": {
       "type": "object",
       "description": "Description of the output value or label.",
       "properties": {
        "numeric": {
         "type": "object",
         "description": "Description of the output values in the data set.",
         "properties": {
          "count": {
           "type": "string",
           "description": "Number of numeric output values in the data set.",
           "format": "int64"
          },
          "mean": {
           "type": "number",
           "description": "Mean of the output values in the data set.",
           "format": "double"
          },
          "variance": {
           "type": "number",
           "description": "Variance of the output values in the data set.",
           "format": "double"
          }
         }
        },
        "text": {
         "type": "array",
         "description": "Description of the output labels in the data set.",
         "items": {
          "type": "object",
          "properties": {
           "count": {
            "type": "string",
            "description": "Number of times the output label occurred in the data set.",
            "format": "int64"
           },
           "value": {
            "type": "string",
            "description": "The output label."
           }
          }
         }
        }
       }
      }
     }
    },
    "errors": {
     "type": "array",
     "description": "List of errors with the data.",
     "items": {
      "type": "object",
      "additionalProperties": {
       "type": "string",
       "description": "Error level followed by a detailed error message."
      }
     }
    },
    "id": {
     "type": "string",
     "description": "The unique name for the predictive model."
    },
    "kind": {
     "type": "string",
     "description": "What kind of resource this is.",
     "default": "prediction#analyze"
    },
    "modelDescription": {
     "type": "object",
     "description": "Description of the model.",
     "properties": {
      "confusionMatrix": {
       "type": "object",
       "description": "An output confusion matrix. This shows an estimate for how this model will do in predictions. This is first indexed by the true class label. For each true class label, this provides a pair {predicted_label, count}, where count is the estimated number of times the model will predict the predicted label given the true label. Will not output if more then 100 classes [Categorical models only].",
       "additionalProperties": {
        "type": "object",
        "additionalProperties": {
         "type": "number",
         "format": "double"
        }
       }
      },
      "confusionMatrixRowTotals": {
       "type": "object",
       "description": "A list of the confusion matrix row totals",
       "additionalProperties": {
        "type": "number",
        "format": "double"
       }
      },
      "modelinfo": {
       "$ref": "Training",
       "description": "Basic information about the model."
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "A URL to re-request this resource."
    }
   }
  },
  "Input": {
   "id": "Input",
   "type": "object",
   "properties": {
    "input": {
     "type": "object",
     "description": "Input to the model for a prediction",
     "properties": {
      "csvInstance": {
       "type": "array",
       "description": "A list of input features, these can be strings or doubles.",
       "items": {
        "type": "any"
       }
      }
     }
    }
   }
  },
  "List": {
   "id": "List",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of models.",
     "items": {
      "$ref": "Training"
     }
    },
    "kind": {
     "type": "string",
     "description": "What kind of resource this is.",
     "default": "prediction#list"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Pagination token to fetch the next page, if one exists."
    },
    "selfLink": {
     "type": "string",
     "description": "A URL to re-request this resource."
    }
   }
  },
  "Output": {
   "id": "Output",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "The unique name for the predictive model."
    },
    "kind": {
     "type": "string",
     "description": "What kind of resource this is.",
     "default": "prediction#output"
    },
    "outputLabel": {
     "type": "string",
     "description": "The most likely class label [Categorical models only]."
    },
    "outputMulti": {
     "type": "array",
     "description": "A list of class labels with their estimated probabilities [Categorical models only].",
     "items": {
      "type": "object",
      "properties": {
       "label": {
        "type": "string",
        "description": "The class label."
       },
       "score": {
        "type": "number",
        "description": "The probability of the class label.",
        "format": "double"
       }
      }
     }
    },
    "outputValue": {
     "type": "number",
     "description": "The estimated regression value [Regression models only].",
     "format": "double"
    },
    "selfLink": {
     "type": "string",
     "description": "A URL to re-request this resource."
    }
   }
  },
  "Training": {
   "id": "Training",
   "type": "object",
   "properties": {
    "created": {
     "type": "string",
     "description": "Insert time of the model (as a RFC 3339 timestamp).",
     "format": "date-time"
    },
    "id": {
     "type": "string",
     "description": "The unique name for the predictive model."
    },
    "kind": {
     "type": "string",
     "description": "What kind of resource this is.",
     "default": "prediction#training"
    },
    "modelInfo": {
     "type": "object",
     "description": "Model metadata.",
     "properties": {
      "classWeightedAccuracy": {
       "type": "number",
       "description": "Estimated accuracy of model taking utility weights into account [Categorical models only].",
       "format": "double"
      },
      "classificationAccuracy": {
       "type": "number",
       "description": "A number between 0.0 and 1.0, where 1.0 is 100% accurate. This is an estimate, based on the amount and quality of the training data, of the estimated prediction accuracy. You can use this is a guide to decide whether the results are accurate enough for your needs. This estimate will be more reliable if your real input data is similar to your training data [Categorical models only].",
       "format": "double"
      },
      "meanSquaredError": {
       "type": "number",
       "description": "An estimated mean squared error. The can be used to measure the quality of the predicted model [Regression models only].",
       "format": "double"
      },
      "modelType": {
       "type": "string",
       "description": "Type of predictive model (CLASSIFICATION or REGRESSION)"
      },
      "numberInstances": {
       "type": "string",
       "description": "Number of valid data instances used in the trained model.",
       "format": "int64"
      },
      "numberLabels": {
       "type": "string",
       "description": "Number of class labels in the trained model [Categorical models only].",
       "format": "int64"
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "A URL to re-request this resource."
    },
    "storageDataLocation": {
     "type": "string",
     "description": "Google storage location of the training data file."
    },
    "storagePMMLLocation": {
     "type": "string",
     "description": "Google storage location of the preprocessing pmml file."
    },
    "storagePMMLModelLocation": {
     "type": "string",
     "description": "Google storage location of the pmml model file."
    },
    "trainingComplete": {
     "type": "string",
     "description": "Training completion time (as a RFC 3339 timestamp).",
     "format": "date-time"
    },
    "trainingStatus": {
     "type": "string",
     "description": "The current status of the training job. This can be one of following: RUNNING; DONE; ERROR; ERROR: TRAINING JOB NOT FOUND"
    },
    "utility": {
     "type": "array",
     "description": "A class weighting function, which allows the importance weights for class labels to be specified [Categorical models only].",
     "items": {
      "type": "object",
      "description": "Class label (string).",
      "additionalProperties": {
       "type": "number",
       "format": "double"
      }
     }
    }
   }
  },
  "Update": {
   "id": "Update",
   "type": "object",
   "properties": {
    "csvInstance": {
     "type": "array",
     "description": "The input features for this instance",
     "items": {
      "type": "any"
     }
    },
    "label": {
     "type": "string",
     "description": "The class label of this instance"
    }
   }
  }
 },
 "resources": {
  "hostedmodels": {
   "methods": {
    "predict": {
     "id": "prediction.hostedmodels.predict",
     "path": "hostedmodels/{hostedModelName}/predict",
     "httpMethod": "POST",
     "description": "Submit input and request an output against a hosted model.",
     "parameters": {
      "hostedModelName": {
       "type": "string",
       "description": "The name of a hosted model.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "hostedModelName"
     ],
     "request": {
      "$ref": "Input"
     },
     "response": {
      "$ref": "Output"
     },
     "scopes": [
      "https://www.googleapis.com/auth/prediction"
     ]
    }
   }
  },
  "trainedmodels": {
   "methods": {
    "analyze": {
     "id": "prediction.trainedmodels.analyze",
     "path": "trainedmodels/{id}/analyze",
     "httpMethod": "GET",
     "description": "Get analysis of the model and the data the model was trained on.",
     "parameters": {
      "id": {
       "type": "string",
       "description": "The unique name for the predictive model.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "response": {
      "$ref": "Analyze"
     },
     "scopes": [
      "https://www.googleapis.com/auth/prediction"
     ]
    },
    "delete": {
     "id": "prediction.trainedmodels.delete",
     "path": "trainedmodels/{id}",
     "httpMethod": "DELETE",
     "description": "Delete a trained model.",
     "parameters": {
      "id": {
       "type": "string",
       "description": "The unique name for the predictive model.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/prediction"
     ]
    },
    "get": {
     "id": "prediction.trainedmodels.get",
     "path": "trainedmodels/{id}",
     "httpMethod": "GET",
     "description": "Check training status of your model.",
     "parameters": {
      "id": {
       "type": "string",
       "description": "The unique name for the predictive model.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "response": {
      "$ref": "Training"
     },
     "scopes": [
      "https://www.googleapis.com/auth/prediction"
     ]
    },
    "insert": {
     "id": "prediction.trainedmodels.insert",
     "path": "trainedmodels",
     "httpMethod": "POST",
     "description": "Begin training your model.",
     "request": {
      "$ref": "Training"
     },
     "response": {
      "$ref": "Training"
     },
     "scopes": [
      "https://www.googleapis.com/auth/devstorage.read_only",
      "https://www.googleapis.com/auth/prediction"
     ]
    },
    "list": {
     "id": "prediction.trainedmodels.list",
     "path": "trainedmodels/list",
     "httpMethod": "GET",
     "description": "List available models.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "format": "uint32",
       "minimum": "0",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Pagination token",
       "location": "query"
      }
     },
     "response": {
      "$ref": "List"
     },
     "scopes": [
      "https://www.googleapis.com/auth/prediction"
     ]
    },
    "predict": {
     "id": "prediction.trainedmodels.predict",
     "path": "trainedmodels/{id}/predict",
     "httpMethod": "POST",
     "description": "Submit model id and request a prediction.",
     "parameters": {
      "id": {
       "type": "string",
       "description": "The unique name for the predictive model.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "request": {
      "$ref": "Input"
     },
     "response": {
      "$ref": "Output"
     },
     "scopes": [
      "https://www.googleapis.com/auth/prediction"
     ]
    },
    "update": {
     "id": "prediction.trainedmodels.update",
     "path": "trainedmodels/{id}",
     "httpMethod": "PUT",
     "description": "Add new data to a trained model.",
     "parameters": {
      "id": {
       "type": "string",
       "description": "The unique name for the predictive model.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "request": {
      "$ref": "Update"
     },
     "response": {
      "$ref": "Training"
     },
     "scopes": [
      "https://www.googleapis.com/auth/prediction"
     ]
    }
   }
  }
 }
}
