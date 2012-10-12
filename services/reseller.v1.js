{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "reseller:v1",
 "name": "reseller",
 "version": "v1",
 "revision": "20120803",
 "title": "Enterprise Apps Reseller API",
 "description": "Lets you create and manage your customers and their subscriptions.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/search-16.gif",
  "x32": "http://www.google.com/images/icons/product/search-32.gif"
 },
 "documentationLink": "https://developers.google.com/google-apps/reseller/",
 "labels": [
  "limited_availability"
 ],
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/apps/reseller/v1/",
 "basePath": "/apps/reseller/v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "apps/reseller/v1/",
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
  "Address": {
   "id": "Address",
   "type": "object",
   "description": "JSON template for address of a customer.",
   "properties": {
    "addressLine1": {
     "type": "string",
     "description": "Address line 1 of the address."
    },
    "addressLine2": {
     "type": "string",
     "description": "Address line 2 of the address."
    },
    "addressLine3": {
     "type": "string",
     "description": "Address line 3 of the address."
    },
    "contactName": {
     "type": "string",
     "description": "Name of the contact person."
    },
    "countryCode": {
     "type": "string",
     "description": "ISO 3166 country code."
    },
    "kind": {
     "type": "string",
     "description": "Identifies the resource as a customer address.",
     "default": "customers#address"
    },
    "locality": {
     "type": "string",
     "description": "Name of the locality. This is in accordance with - http://portablecontacts.net/draft-spec.html#address_element."
    },
    "organizationName": {
     "type": "string",
     "description": "Name of the organization."
    },
    "postalCode": {
     "type": "string",
     "description": "The postal code. This is in accordance with - http://portablecontacts.net/draft-spec.html#address_element."
    },
    "region": {
     "type": "string",
     "description": "Name of the region. This is in accordance with - http://portablecontacts.net/draft-spec.html#address_element."
    }
   }
  },
  "ChangePlanRequest": {
   "id": "ChangePlanRequest",
   "type": "object",
   "description": "JSON template for the ChangePlan rpc request.",
   "properties": {
    "kind": {
     "type": "string",
     "description": "Identifies the resource as a subscription change plan request.",
     "default": "subscriptions#changePlanRequest"
    },
    "planName": {
     "type": "string",
     "description": "Name of the plan to change to."
    },
    "purchaseOrderId": {
     "type": "string",
     "description": "Purchase order id for your order tracking purposes."
    },
    "seats": {
     "$ref": "Seats",
     "description": "Number/Limit of seats in the new plan."
    }
   }
  },
  "Customer": {
   "id": "Customer",
   "type": "object",
   "description": "JSON template for a customer.",
   "properties": {
    "alternateEmail": {
     "type": "string",
     "description": "The alternate email of the customer."
    },
    "customerDomain": {
     "type": "string",
     "description": "The domain name of the customer."
    },
    "customerId": {
     "type": "string",
     "description": "The id of the customer."
    },
    "kind": {
     "type": "string",
     "description": "Identifies the resource as a customer.",
     "default": "reseller#customer"
    },
    "phoneNumber": {
     "type": "string",
     "description": "The phone number of the customer."
    },
    "postalAddress": {
     "$ref": "Address",
     "description": "The postal address of the customer."
    }
   }
  },
  "RenewalSettings": {
   "id": "RenewalSettings",
   "type": "object",
   "description": "JSON template for a subscription renewal settings.",
   "properties": {
    "kind": {
     "type": "string",
     "description": "Identifies the resource as a subscription renewal setting.",
     "default": "subscriptions#renewalSettings"
    },
    "renewalType": {
     "type": "string",
     "description": "Subscription renewal type."
    }
   }
  },
  "Seats": {
   "id": "Seats",
   "type": "object",
   "description": "JSON template for subscription seats.",
   "properties": {
    "kind": {
     "type": "string",
     "description": "Identifies the resource as a subscription change plan request.",
     "default": "subscriptions#seats"
    },
    "maximumNumberOfSeats": {
     "type": "integer",
     "description": "Maximum number of seats that can be purchased. This needs to be provided only for a non-commitment plan. For a commitment plan it is decided by the contract.",
     "format": "int32"
    },
    "numberOfSeats": {
     "type": "integer",
     "description": "Number of seats to purchase. This is applicable only for a commitment plan.",
     "format": "int32"
    }
   }
  },
  "Subscription": {
   "id": "Subscription",
   "type": "object",
   "description": "JSON template for a subscription.",
   "properties": {
    "creationTime": {
     "type": "string",
     "description": "Creation time of this subscription in milliseconds since Unix epoch.",
     "format": "int64"
    },
    "customerId": {
     "type": "string",
     "description": "The id of the customer to whom the subscription belongs."
    },
    "kind": {
     "type": "string",
     "description": "Identifies the resource as a Subscription.",
     "default": "reseller#subscription"
    },
    "plan": {
     "type": "object",
     "description": "Plan details of the subscription",
     "properties": {
      "commitmentInterval": {
       "type": "object",
       "description": "Interval of the commitment if it is a commitment plan.",
       "properties": {
        "endTime": {
         "type": "string",
         "description": "End time of the commitment interval in milliseconds since Unix epoch.",
         "format": "int64"
        },
        "startTime": {
         "type": "string",
         "description": "Start time of the commitment interval in milliseconds since Unix epoch.",
         "format": "int64"
        }
       }
      },
      "isCommitmentPlan": {
       "type": "boolean",
       "description": "Whether the plan is a commitment plan or not."
      },
      "planName": {
       "type": "string",
       "description": "The plan name of this subscription's plan."
      }
     }
    },
    "purchaseOrderId": {
     "type": "string",
     "description": "Purchase order id for your order tracking purposes."
    },
    "renewalSettings": {
     "$ref": "RenewalSettings",
     "description": "Renewal settings of the subscription."
    },
    "seats": {
     "$ref": "Seats",
     "description": "Number/Limit of seats in the new plan."
    },
    "skuId": {
     "type": "string",
     "description": "Name of the sku for which this subscription is purchased."
    },
    "subscriptionId": {
     "type": "string",
     "description": "The id of the subscription."
    },
    "trialSettings": {
     "type": "object",
     "description": "Trial Settings of the subscription.",
     "properties": {
      "isInTrial": {
       "type": "boolean",
       "description": "Whether the subscription is in trial."
      },
      "trialEndTime": {
       "type": "string",
       "description": "End time of the trial in milliseconds since Unix epoch.",
       "format": "int64"
      }
     }
    }
   }
  },
  "Subscriptions": {
   "id": "Subscriptions",
   "type": "object",
   "description": "JSON template for a subscription list.",
   "properties": {
    "kind": {
     "type": "string",
     "description": "Identifies the resource as a collection of subscriptions.",
     "default": "reseller#subscriptions"
    },
    "nextPageToken": {
     "type": "string",
     "description": "The continuation token, used to page through large result sets. Provide this value in a subsequent request to return the next page of results."
    },
    "subscriptions": {
     "type": "array",
     "description": "The subscriptions in this page of results.",
     "items": {
      "$ref": "Subscription"
     }
    }
   }
  }
 },
 "resources": {
  "customers": {
   "methods": {
    "get": {
     "id": "reseller.customers.get",
     "path": "customers/{customerId}",
     "httpMethod": "GET",
     "description": "Gets a customer resource if one exists and is owned by the reseller.",
     "parameters": {
      "customerId": {
       "type": "string",
       "description": "Id of the Customer",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "customerId"
     ],
     "response": {
      "$ref": "Customer"
     }
    },
    "insert": {
     "id": "reseller.customers.insert",
     "path": "customers",
     "httpMethod": "POST",
     "description": "Creates a customer resource if one does not already exist.",
     "parameters": {
      "customerAuthToken": {
       "type": "string",
       "description": "An auth token needed for inserting a customer for which domain already exists. Can be generated at https://www.google.com/a/cpanel//TransferToken. Optional.",
       "location": "query"
      }
     },
     "request": {
      "$ref": "Customer"
     },
     "response": {
      "$ref": "Customer"
     }
    },
    "patch": {
     "id": "reseller.customers.patch",
     "path": "customers/{customerId}",
     "httpMethod": "PATCH",
     "description": "Update a customer resource if one it exists and is owned by the reseller. This method supports patch semantics.",
     "parameters": {
      "customerId": {
       "type": "string",
       "description": "Id of the Customer",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "customerId"
     ],
     "request": {
      "$ref": "Customer"
     },
     "response": {
      "$ref": "Customer"
     }
    },
    "update": {
     "id": "reseller.customers.update",
     "path": "customers/{customerId}",
     "httpMethod": "PUT",
     "description": "Update a customer resource if one it exists and is owned by the reseller.",
     "parameters": {
      "customerId": {
       "type": "string",
       "description": "Id of the Customer",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "customerId"
     ],
     "request": {
      "$ref": "Customer"
     },
     "response": {
      "$ref": "Customer"
     }
    }
   }
  },
  "subscriptions": {
   "methods": {
    "changePlan": {
     "id": "reseller.subscriptions.changePlan",
     "path": "customers/{customerId}/subscriptions/{subscriptionId}/changePlan",
     "httpMethod": "POST",
     "description": "Changes the plan of a subscription",
     "parameters": {
      "customerId": {
       "type": "string",
       "description": "Id of the Customer",
       "required": true,
       "location": "path"
      },
      "subscriptionId": {
       "type": "string",
       "description": "Id of the subscription, which is unique for a customer",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "customerId",
      "subscriptionId"
     ],
     "request": {
      "$ref": "ChangePlanRequest"
     },
     "response": {
      "$ref": "Subscription"
     }
    },
    "changeRenewalSettings": {
     "id": "reseller.subscriptions.changeRenewalSettings",
     "path": "customers/{customerId}/subscriptions/{subscriptionId}/changeRenewalSettings",
     "httpMethod": "POST",
     "description": "Changes the renewal settings of a subscription",
     "parameters": {
      "customerId": {
       "type": "string",
       "description": "Id of the Customer",
       "required": true,
       "location": "path"
      },
      "subscriptionId": {
       "type": "string",
       "description": "Id of the subscription, which is unique for a customer",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "customerId",
      "subscriptionId"
     ],
     "request": {
      "$ref": "RenewalSettings"
     },
     "response": {
      "$ref": "Subscription"
     }
    },
    "changeSeats": {
     "id": "reseller.subscriptions.changeSeats",
     "path": "customers/{customerId}/subscriptions/{subscriptionId}/changeSeats",
     "httpMethod": "POST",
     "description": "Changes the seats configuration of a subscription",
     "parameters": {
      "customerId": {
       "type": "string",
       "description": "Id of the Customer",
       "required": true,
       "location": "path"
      },
      "subscriptionId": {
       "type": "string",
       "description": "Id of the subscription, which is unique for a customer",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "customerId",
      "subscriptionId"
     ],
     "request": {
      "$ref": "Seats"
     },
     "response": {
      "$ref": "Subscription"
     }
    },
    "delete": {
     "id": "reseller.subscriptions.delete",
     "path": "customers/{customerId}/subscriptions/{subscriptionId}",
     "httpMethod": "DELETE",
     "description": "Cancels/Downgrades a subscription.",
     "parameters": {
      "customerId": {
       "type": "string",
       "description": "Id of the Customer",
       "required": true,
       "location": "path"
      },
      "deletionType": {
       "type": "string",
       "description": "Whether the subscription is to be fully cancelled or downgraded",
       "required": true,
       "enum": [
        "cancel",
        "downgrade",
        "suspend"
       ],
       "enumDescriptions": [
        "Cancels the subscription immediately",
        "Downgrades a Google Apps for Business subscription to Google Apps",
        "Suspends the subscriptions for 4 days before cancelling it"
       ],
       "location": "query"
      },
      "subscriptionId": {
       "type": "string",
       "description": "Id of the subscription, which is unique for a customer",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "customerId",
      "subscriptionId",
      "deletionType"
     ]
    },
    "get": {
     "id": "reseller.subscriptions.get",
     "path": "customers/{customerId}/subscriptions/{subscriptionId}",
     "httpMethod": "GET",
     "description": "Gets a subscription of the customer.",
     "parameters": {
      "customerId": {
       "type": "string",
       "description": "Id of the Customer",
       "required": true,
       "location": "path"
      },
      "subscriptionId": {
       "type": "string",
       "description": "Id of the subscription, which is unique for a customer",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "customerId",
      "subscriptionId"
     ],
     "response": {
      "$ref": "Subscription"
     }
    },
    "insert": {
     "id": "reseller.subscriptions.insert",
     "path": "customers/{customerId}/subscriptions",
     "httpMethod": "POST",
     "description": "Creates/Transfers a subscription for the customer.",
     "parameters": {
      "customerAuthToken": {
       "type": "string",
       "description": "An auth token needed for transferring a subscription. Can be generated at https://www.google.com/a/cpanel/customer-domain/TransferToken. Optional.",
       "location": "query"
      },
      "customerId": {
       "type": "string",
       "description": "Id of the Customer",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "customerId"
     ],
     "request": {
      "$ref": "Subscription"
     },
     "response": {
      "$ref": "Subscription"
     }
    },
    "list": {
     "id": "reseller.subscriptions.list",
     "path": "subscriptions",
     "httpMethod": "GET",
     "description": "Lists subscriptions of a reseller, optionally filtered by a customer name prefix.",
     "parameters": {
      "customerNamePrefix": {
       "type": "string",
       "description": "Prefix of the customer's domain name by which the subscriptions should be filtered. Optional",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "format": "uint32",
       "minimum": "1",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Token to specify next page in the list",
       "location": "query"
      }
     },
     "response": {
      "$ref": "Subscriptions"
     }
    },
    "startPaidService": {
     "id": "reseller.subscriptions.startPaidService",
     "path": "customers/{customerId}/subscriptions/{subscriptionId}/startPaidService",
     "httpMethod": "POST",
     "description": "Starts paid service of a trial subscription",
     "parameters": {
      "customerId": {
       "type": "string",
       "description": "Id of the Customer",
       "required": true,
       "location": "path"
      },
      "subscriptionId": {
       "type": "string",
       "description": "Id of the subscription, which is unique for a customer",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "customerId",
      "subscriptionId"
     ],
     "response": {
      "$ref": "Subscription"
     }
    }
   }
  }
 }
}
