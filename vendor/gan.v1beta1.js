{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "gan:v1beta1",
 "name": "gan",
 "version": "v1beta1",
 "revision": "20120904",
 "title": "Google Affiliate Network API",
 "description": "Lets you have programmatic access to your Google Affiliate Network data.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/affiliatenetwork-16.png",
  "x32": "http://www.google.com/images/icons/product/affiliatenetwork-32.png"
 },
 "documentationLink": "https://developers.google.com/affiliate-network/",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/gan/v1beta1/",
 "basePath": "/gan/v1beta1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "gan/v1beta1/",
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
    "https://www.googleapis.com/auth/gan": {
     "description": "Manage your GAN data"
    },
    "https://www.googleapis.com/auth/gan.readonly": {
     "description": "View your GAN data"
    }
   }
  }
 },
 "schemas": {
  "Advertiser": {
   "id": "Advertiser",
   "type": "object",
   "description": "An AdvertiserResource.",
   "properties": {
    "allowPublisherCreatedLinks": {
     "type": "boolean",
     "description": "True if the advertiser allows publisher created links, otherwise false."
    },
    "category": {
     "type": "string",
     "description": "Category that this advertiser belongs to. A valid list of categories can be found here: http://www.google.com/support/affiliatenetwork/advertiser/bin/answer.py?hl=en&answer=107581"
    },
    "commissionDuration": {
     "type": "integer",
     "description": "The longest possible length of a commission (how long the cookies on the customer's browser last before they expire).",
     "format": "int32"
    },
    "contactEmail": {
     "type": "string",
     "description": "Email that this advertiser would like publishers to contact them with."
    },
    "contactPhone": {
     "type": "string",
     "description": "Phone that this advertiser would like publishers to contact them with."
    },
    "defaultLinkId": {
     "type": "string",
     "description": "The default link id for this advertiser.",
     "format": "int64"
    },
    "description": {
     "type": "string",
     "description": "Description of the website the advertiser advertises from."
    },
    "epcNinetyDayAverage": {
     "$ref": "Money",
     "description": "The sum of fees paid to publishers divided by the total number of clicks over the past three months. Values are multiplied by 100 for display purposes."
    },
    "epcSevenDayAverage": {
     "$ref": "Money",
     "description": "The sum of fees paid to publishers divided by the total number of clicks over the past seven days. Values are multiplied by 100 for display purposes."
    },
    "id": {
     "type": "string",
     "description": "The ID of this advertiser.",
     "format": "int64"
    },
    "item": {
     "$ref": "Advertiser",
     "description": "The requested advertiser."
    },
    "joinDate": {
     "type": "string",
     "description": "Date that this advertiser was approved as a Google Affiliate Network advertiser.",
     "format": "date-time"
    },
    "kind": {
     "type": "string",
     "description": "The kind for an advertiser.",
     "default": "gan#advertiser"
    },
    "logoUrl": {
     "type": "string",
     "description": "URL to the logo this advertiser uses on the Google Affiliate Network."
    },
    "name": {
     "type": "string",
     "description": "The name of this advertiser."
    },
    "payoutRank": {
     "type": "string",
     "description": "A rank based on commissions paid to publishers over the past 90 days. A number between 1 and 4 where 4 means the top quartile (most money paid) and 1 means the bottom quartile (least money paid)."
    },
    "productFeedsEnabled": {
     "type": "boolean",
     "description": "Allows advertisers to submit product listings to Google Product Search."
    },
    "redirectDomains": {
     "type": "array",
     "description": "List of redirect URLs for this advertiser",
     "items": {
      "type": "string"
     }
    },
    "siteUrl": {
     "type": "string",
     "description": "URL of the website this advertiser advertises from."
    },
    "status": {
     "type": "string",
     "description": "The status of the requesting publisher's relationship this advertiser."
    }
   }
  },
  "Advertisers": {
   "id": "Advertisers",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "The advertiser list.",
     "items": {
      "$ref": "Advertiser"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind for a page of advertisers.",
     "default": "gan#advertisers"
    },
    "nextPageToken": {
     "type": "string",
     "description": "The 'pageToken' to pass to the next request to get the next page, if there are more to retrieve."
    }
   }
  },
  "CcOffer": {
   "id": "CcOffer",
   "type": "object",
   "description": "A credit card offer. There are many possible result fields. We provide two different views of the data, or \"projections.\" The \"full\" projection includes every result field. And the \"summary\" projection, which is the default, includes a smaller subset of the fields. The fields included in the summary projection are marked as such in their descriptions.",
   "properties": {
    "additionalCardBenefits": {
     "type": "array",
     "description": "More marketing copy about the card's benefits. A summary field.",
     "items": {
      "type": "string"
     }
    },
    "additionalCardHolderFee": {
     "type": "string",
     "description": "Any extra fees levied on card holders."
    },
    "ageMinimum": {
     "type": "number",
     "description": "The youngest a recipient of this card may be.",
     "format": "double"
    },
    "ageMinimumDetails": {
     "type": "string",
     "description": "Text describing the details of the age minimum restriction."
    },
    "annualFee": {
     "type": "number",
     "description": "The ongoing annual fee, in dollars.",
     "format": "double"
    },
    "annualFeeDisplay": {
     "type": "string",
     "description": "Text describing the annual fee, including any difference for the first year. A summary field."
    },
    "annualRewardMaximum": {
     "type": "number",
     "description": "The largest number of units you may accumulate in a year.",
     "format": "double"
    },
    "approvedCategories": {
     "type": "array",
     "description": "Possible categories for this card, eg \"Low Interest\" or \"Good.\" A summary field.",
     "items": {
      "type": "string"
     }
    },
    "aprDisplay": {
     "type": "string",
     "description": "Text describing the purchase APR. A summary field."
    },
    "balanceComputationMethod": {
     "type": "string",
     "description": "Text describing how the balance is computed. A summary field."
    },
    "balanceTransferTerms": {
     "type": "string",
     "description": "Text describing the terms for balance transfers. A summary field."
    },
    "bonusRewards": {
     "type": "array",
     "description": "For cards with rewards programs, extra circumstances whereby additional rewards may be granted.",
     "items": {
      "type": "object",
      "properties": {
       "amount": {
        "type": "number",
        "description": "How many units of reward will be granted.",
        "format": "double"
       },
       "details": {
        "type": "string",
        "description": "The circumstances under which this rule applies, for example, booking a flight via Orbitz."
       }
      }
     }
    },
    "carRentalInsurance": {
     "type": "string",
     "description": "If you get coverage when you use the card for the given activity, this field describes it."
    },
    "cardBenefits": {
     "type": "array",
     "description": "A list of what the issuer thinks are the most important benefits of the card. Usually summarizes the rewards program, if there is one. A summary field.",
     "items": {
      "type": "string"
     }
    },
    "cardName": {
     "type": "string",
     "description": "The issuer's name for the card, including any trademark or service mark designators. A summary field."
    },
    "cardType": {
     "type": "string",
     "description": "What kind of credit card this is, for example secured or unsecured."
    },
    "cashAdvanceTerms": {
     "type": "string",
     "description": "Text describing the terms for cash advances. A summary field."
    },
    "creditLimitMax": {
     "type": "number",
     "description": "The high end for credit limits the issuer imposes on recipients of this card.",
     "format": "double"
    },
    "creditLimitMin": {
     "type": "number",
     "description": "The low end for credit limits the issuer imposes on recipients of this card.",
     "format": "double"
    },
    "creditRatingDisplay": {
     "type": "string",
     "description": "Text describing the credit ratings required for recipients of this card, for example \"Excellent/Good.\" A summary field."
    },
    "defaultFees": {
     "type": "array",
     "description": "Fees for defaulting on your payments.",
     "items": {
      "type": "object",
      "properties": {
       "category": {
        "type": "string",
        "description": "The type of charge, for example Purchases."
       },
       "maxRate": {
        "type": "number",
        "description": "The highest rate the issuer may charge for defaulting on debt in this category. Expressed as an absolute number, not as a percentage.",
        "format": "double"
       },
       "minRate": {
        "type": "number",
        "description": "The lowest rate the issuer may charge for defaulting on debt in this category. Expressed as an absolute number, not as a percentage.",
        "format": "double"
       },
       "rateType": {
        "type": "string",
        "description": "Fixed or variable."
       }
      }
     }
    },
    "disclaimer": {
     "type": "string",
     "description": "A notice that, if present, is referenced via an asterisk by many of the other summary fields. If this field is present, it will always start with an asterisk (\"*\"), and must be prominently displayed with the offer. A summary field."
    },
    "emergencyInsurance": {
     "type": "string",
     "description": "If you get coverage when you use the card for the given activity, this field describes it."
    },
    "existingCustomerOnly": {
     "type": "boolean",
     "description": "Whether this card is only available to existing customers of the issuer."
    },
    "extendedWarranty": {
     "type": "string",
     "description": "If you get coverage when you use the card for the given activity, this field describes it."
    },
    "firstYearAnnualFee": {
     "type": "number",
     "description": "The annual fee for the first year, if different from the ongoing fee. Optional.",
     "format": "double"
    },
    "flightAccidentInsurance": {
     "type": "string",
     "description": "If you get coverage when you use the card for the given activity, this field describes it."
    },
    "foreignCurrencyTransactionFee": {
     "type": "string",
     "description": "Fee for each transaction involving a foreign currency."
    },
    "fraudLiability": {
     "type": "string",
     "description": "If you get coverage when you use the card for the given activity, this field describes it."
    },
    "gracePeriodDisplay": {
     "type": "string",
     "description": "Text describing the grace period before finance charges apply. A summary field."
    },
    "imageUrl": {
     "type": "string",
     "description": "The link to the image of the card that is shown on Connect Commerce. A summary field."
    },
    "initialSetupAndProcessingFee": {
     "type": "string",
     "description": "Fee for setting up the card."
    },
    "introBalanceTransferTerms": {
     "type": "string",
     "description": "Text describing the terms for introductory period balance transfers. A summary field."
    },
    "introCashAdvanceTerms": {
     "type": "string",
     "description": "Text describing the terms for introductory period cash advances. A summary field."
    },
    "introPurchaseTerms": {
     "type": "string",
     "description": "Text describing the terms for introductory period purchases. A summary field."
    },
    "issuer": {
     "type": "string",
     "description": "Name of card issuer. A summary field."
    },
    "issuerId": {
     "type": "string",
     "description": "The Google Affiliate Network ID of the advertiser making this offer."
    },
    "issuerWebsite": {
     "type": "string",
     "description": "The generic link to the issuer's site."
    },
    "kind": {
     "type": "string",
     "description": "The kind for one credit card offer. A summary field.",
     "default": "gan#ccOffer"
    },
    "landingPageUrl": {
     "type": "string",
     "description": "The link to the issuer's page for this card. A summary field."
    },
    "latePaymentFee": {
     "type": "string",
     "description": "Text describing how much a late payment will cost, eg \"up to $35.\" A summary field."
    },
    "luggageInsurance": {
     "type": "string",
     "description": "If you get coverage when you use the card for the given activity, this field describes it."
    },
    "maxPurchaseRate": {
     "type": "number",
     "description": "The highest interest rate the issuer charges on this card. Expressed as an absolute number, not as a percentage.",
     "format": "double"
    },
    "minPurchaseRate": {
     "type": "number",
     "description": "The lowest interest rate the issuer charges on this card. Expressed as an absolute number, not as a percentage.",
     "format": "double"
    },
    "minimumFinanceCharge": {
     "type": "string",
     "description": "Text describing how much missing the grace period will cost."
    },
    "network": {
     "type": "string",
     "description": "Which network (eg Visa) the card belongs to. A summary field."
    },
    "offerId": {
     "type": "string",
     "description": "This offer's ID. A summary field."
    },
    "offersImmediateCashReward": {
     "type": "boolean",
     "description": "Whether a cash reward program lets you get cash back sooner than end of year or other longish period."
    },
    "overLimitFee": {
     "type": "string",
     "description": "Fee for exceeding the card's charge limit."
    },
    "prohibitedCategories": {
     "type": "array",
     "description": "Categories in which the issuer does not wish the card to be displayed. A summary field.",
     "items": {
      "type": "string"
     }
    },
    "purchaseRateAdditionalDetails": {
     "type": "string",
     "description": "Text describing any additional details for the purchase rate. A summary field."
    },
    "purchaseRateType": {
     "type": "string",
     "description": "Fixed or variable."
    },
    "returnedPaymentFee": {
     "type": "string",
     "description": "Text describing the fee for a payment that doesn't clear. A summary field."
    },
    "rewardPartner": {
     "type": "string",
     "description": "The company that redeems the rewards, if different from the issuer."
    },
    "rewardUnit": {
     "type": "string",
     "description": "For cards with rewards programs, the unit of reward. For example, miles, cash back, points."
    },
    "rewards": {
     "type": "array",
     "description": "For cards with rewards programs, detailed rules about how the program works.",
     "items": {
      "type": "object",
      "properties": {
       "additionalDetails": {
        "type": "string",
        "description": "Other limits, for example, if this rule only applies during an introductory period."
       },
       "amount": {
        "type": "number",
        "description": "The number of units rewarded per purchase dollar.",
        "format": "double"
       },
       "category": {
        "type": "string",
        "description": "The kind of purchases covered by this rule."
       },
       "expirationMonths": {
        "type": "number",
        "description": "How long rewards granted by this rule last.",
        "format": "double"
       },
       "maxRewardTier": {
        "type": "number",
        "description": "The maximum purchase amount in the given category for this rule to apply.",
        "format": "double"
       },
       "minRewardTier": {
        "type": "number",
        "description": "The minimum purchase amount in the given category before this rule applies.",
        "format": "double"
       }
      }
     }
    },
    "rewardsExpire": {
     "type": "boolean",
     "description": "Whether accumulated rewards ever expire."
    },
    "rewardsHaveBlackoutDates": {
     "type": "boolean",
     "description": "For airline miles rewards, tells whether blackout dates apply to the miles."
    },
    "statementCopyFee": {
     "type": "string",
     "description": "Fee for requesting a copy of your statement."
    },
    "trackingUrl": {
     "type": "string",
     "description": "The link to ping to register a click on this offer. A summary field."
    },
    "travelInsurance": {
     "type": "string",
     "description": "If you get coverage when you use the card for the given activity, this field describes it."
    },
    "variableRatesLastUpdated": {
     "type": "string",
     "description": "When variable rates were last updated."
    },
    "variableRatesUpdateFrequency": {
     "type": "string",
     "description": "How often variable rates are updated."
    }
   }
  },
  "CcOffers": {
   "id": "CcOffers",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "The credit card offers.",
     "items": {
      "$ref": "CcOffer"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind for a page of credit card offers.",
     "default": "gan#ccOffers"
    }
   }
  },
  "Event": {
   "id": "Event",
   "type": "object",
   "description": "An EventResource.",
   "properties": {
    "advertiserId": {
     "type": "string",
     "description": "The ID of advertiser for this event.",
     "format": "int64"
    },
    "advertiserName": {
     "type": "string",
     "description": "The name of the advertiser for this event."
    },
    "chargeId": {
     "type": "string",
     "description": "The charge ID for this event. Only returned for charge events."
    },
    "chargeType": {
     "type": "string",
     "description": "Charge type of the event (other|slotting_fee|monthly_minimum|tier_bonus|debit|credit). Only returned for charge events."
    },
    "commissionableSales": {
     "$ref": "Money",
     "description": "Amount of money exchanged during the transaction. Only returned for charge and conversion events."
    },
    "earnings": {
     "$ref": "Money",
     "description": "Earnings by the publisher."
    },
    "eventDate": {
     "type": "string",
     "description": "The date-time this event was initiated as a RFC 3339 date-time value.",
     "format": "date-time"
    },
    "kind": {
     "type": "string",
     "description": "The kind for one event.",
     "default": "gan#event"
    },
    "memberId": {
     "type": "string",
     "description": "The ID of the member attached to this event. Only returned for conversion events."
    },
    "modifyDate": {
     "type": "string",
     "description": "The date-time this event was last modified as a RFC 3339 date-time value.",
     "format": "date-time"
    },
    "networkFee": {
     "$ref": "Money",
     "description": "Fee that the advertiser paid to the Google Affiliate Network."
    },
    "orderId": {
     "type": "string",
     "description": "The order ID for this event. Only returned for conversion events."
    },
    "products": {
     "type": "array",
     "description": "Products associated with the event.",
     "items": {
      "type": "object",
      "properties": {
       "categoryId": {
        "type": "string",
        "description": "Id of the category this product belongs to."
       },
       "categoryName": {
        "type": "string",
        "description": "Name of the category this product belongs to."
       },
       "earnings": {
        "$ref": "Money",
        "description": "Amount earned by the publisher on this product."
       },
       "networkFee": {
        "$ref": "Money",
        "description": "Fee that the advertiser paid to the Google Affiliate Network for this product."
       },
       "publisherFee": {
        "$ref": "Money",
        "description": "Fee that the advertiser paid to the publisehr for this product."
       },
       "quantity": {
        "type": "string",
        "description": "Quantity of this product bought/exchanged.",
        "format": "int64"
       },
       "sku": {
        "type": "string",
        "description": "Sku of this product."
       },
       "skuName": {
        "type": "string",
        "description": "Sku name of this product."
       },
       "unitPrice": {
        "$ref": "Money",
        "description": "Price per unit of this product."
       }
      }
     }
    },
    "publisherFee": {
     "$ref": "Money",
     "description": "Fee that the advertiser paid to the publisher."
    },
    "publisherId": {
     "type": "string",
     "description": "The ID of the publisher for this event.",
     "format": "int64"
    },
    "publisherName": {
     "type": "string",
     "description": "The name of the publisher for this event."
    },
    "status": {
     "type": "string",
     "description": "Status of the event (active|canceled). Only returned for charge and conversion events."
    },
    "type": {
     "type": "string",
     "description": "Type of the event (action|transaction|charge)."
    }
   }
  },
  "Events": {
   "id": "Events",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "The event list.",
     "items": {
      "$ref": "Event"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind for a page of events.",
     "default": "gan#events"
    },
    "nextPageToken": {
     "type": "string",
     "description": "The 'pageToken' to pass to the next request to get the next page, if there are more to retrieve."
    }
   }
  },
  "Link": {
   "id": "Link",
   "type": "object",
   "description": "A LinkResource.",
   "properties": {
    "advertiserId": {
     "type": "string",
     "description": "The advertiser id for the advertiser who owns this link.",
     "format": "int64"
    },
    "authorship": {
     "type": "string",
     "description": "Authorship"
    },
    "availability": {
     "type": "string",
     "description": "Availability."
    },
    "clickTrackingUrl": {
     "type": "string",
     "description": "Tracking url for clicks."
    },
    "createDate": {
     "type": "string",
     "description": "Date that this link was created.",
     "format": "date-time"
    },
    "description": {
     "type": "string",
     "description": "Description."
    },
    "destinationUrl": {
     "type": "string",
     "description": "The destination URL for the link."
    },
    "duration": {
     "type": "string",
     "description": "Duration"
    },
    "endDate": {
     "type": "string",
     "description": "Date that this link becomes inactive.",
     "format": "date-time"
    },
    "id": {
     "type": "string",
     "description": "The ID of this link.",
     "format": "int64"
    },
    "imageAltText": {
     "type": "string",
     "description": "image alt text."
    },
    "impressionTrackingUrl": {
     "type": "string",
     "description": "Tracking url for impressions."
    },
    "isActive": {
     "type": "boolean",
     "description": "Flag for if this link is active."
    },
    "kind": {
     "type": "string",
     "description": "The kind for one entity.",
     "default": "gan#link"
    },
    "linkType": {
     "type": "string",
     "description": "The link type."
    },
    "name": {
     "type": "string",
     "description": "The logical name for this link."
    },
    "promotionType": {
     "type": "string",
     "description": "Promotion Type"
    },
    "startDate": {
     "type": "string",
     "description": "Date that this link becomes active.",
     "format": "date-time"
    }
   }
  },
  "Links": {
   "id": "Links",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "The links.",
     "items": {
      "$ref": "Link"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind for a page of links.",
     "default": "gan#links"
    },
    "nextPageToken": {
     "type": "string",
     "description": "The next page token."
    }
   }
  },
  "Money": {
   "id": "Money",
   "type": "object",
   "description": "An ApiMoneyProto.",
   "properties": {
    "amount": {
     "type": "number",
     "description": "The amount of money.",
     "format": "double"
    },
    "currencyCode": {
     "type": "string",
     "description": "The 3-letter code of the currency in question."
    }
   }
  },
  "Publisher": {
   "id": "Publisher",
   "type": "object",
   "description": "A PublisherResource.",
   "properties": {
    "classification": {
     "type": "string",
     "description": "Classification that this publisher belongs to. See this link for all publisher classifications: http://www.google.com/support/affiliatenetwork/advertiser/bin/answer.py?hl=en&answer=107625&ctx=cb&src=cb&cbid=-k5fihzthfaik&cbrank=4"
    },
    "epcNinetyDayAverage": {
     "$ref": "Money",
     "description": "The sum of fees paid to this publisher divided by the total number of clicks over the past three months. Values are multiplied by 100 for display purposes."
    },
    "epcSevenDayAverage": {
     "$ref": "Money",
     "description": "The sum of fees paid to this publisher divided by the total number of clicks over the past seven days. Values are multiplied by 100 for display purposes."
    },
    "id": {
     "type": "string",
     "description": "The ID of this publisher.",
     "format": "int64"
    },
    "item": {
     "$ref": "Publisher",
     "description": "The requested publisher."
    },
    "joinDate": {
     "type": "string",
     "description": "Date that this publisher was approved as a Google Affiliate Network publisher.",
     "format": "date-time"
    },
    "kind": {
     "type": "string",
     "description": "The kind for a publisher.",
     "default": "gan#publisher"
    },
    "name": {
     "type": "string",
     "description": "The name of this publisher."
    },
    "payoutRank": {
     "type": "string",
     "description": "A rank based on commissions paid to this publisher over the past 90 days. A number between 1 and 4 where 4 means the top quartile (most money paid) and 1 means the bottom quartile (least money paid)."
    },
    "sites": {
     "type": "array",
     "description": "Websites that this publisher uses to advertise.",
     "items": {
      "type": "string"
     }
    },
    "status": {
     "type": "string",
     "description": "The status of the requesting advertiser's relationship with this publisher."
    }
   }
  },
  "Publishers": {
   "id": "Publishers",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "The entity list.",
     "items": {
      "$ref": "Publisher"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind for a page of entities.",
     "default": "gan#publishers"
    },
    "nextPageToken": {
     "type": "string",
     "description": "The 'pageToken' to pass to the next request to get the next page, if there are more to retrieve."
    }
   }
  }
 },
 "resources": {
  "advertisers": {
   "methods": {
    "get": {
     "id": "gan.advertisers.get",
     "path": "{role}/{roleId}/advertiser",
     "httpMethod": "GET",
     "description": "Retrieves data about a single advertiser if that the requesting advertiser/publisher has access to it. Only publishers can lookup advertisers. Advertisers can request information about themselves by omitting the advertiserId query parameter.",
     "parameters": {
      "advertiserId": {
       "type": "string",
       "description": "The ID of the advertiser to look up. Optional.",
       "location": "query"
      },
      "role": {
       "type": "string",
       "description": "The role of the requester. Valid values: 'advertisers' or 'publishers'.",
       "required": true,
       "enum": [
        "advertisers",
        "publishers"
       ],
       "enumDescriptions": [
        "The requester is requesting as an advertiser.",
        "The requester is requesting as a publisher."
       ],
       "location": "path"
      },
      "roleId": {
       "type": "string",
       "description": "The ID of the requesting advertiser or publisher.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "role",
      "roleId"
     ],
     "response": {
      "$ref": "Advertiser"
     },
     "scopes": [
      "https://www.googleapis.com/auth/gan",
      "https://www.googleapis.com/auth/gan.readonly"
     ]
    },
    "list": {
     "id": "gan.advertisers.list",
     "path": "{role}/{roleId}/advertisers",
     "httpMethod": "GET",
     "description": "Retrieves data about all advertisers that the requesting advertiser/publisher has access to.",
     "parameters": {
      "advertiserCategory": {
       "type": "string",
       "description": "Caret(^) delimted list of advertiser categories. Valid categories are defined here: http://www.google.com/support/affiliatenetwork/advertiser/bin/answer.py?hl=en&answer=107581. Filters out all advertisers not in one of the given advertiser categories. Optional.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Max number of items to return in this page. Optional. Defaults to 20.",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "minNinetyDayEpc": {
       "type": "number",
       "description": "Filters out all advertisers that have a ninety day EPC average lower than the given value (inclusive). Min value: 0.0. Optional.",
       "format": "double",
       "location": "query"
      },
      "minPayoutRank": {
       "type": "integer",
       "description": "A value between 1 and 4, where 1 represents the quartile of advertisers with the lowest ranks and 4 represents the quartile of advertisers with the highest ranks. Filters out all advertisers with a lower rank than the given quartile. For example if a 2 was given only advertisers with a payout rank of 25 or higher would be included. Optional.",
       "format": "int32",
       "minimum": "1",
       "maximum": "4",
       "location": "query"
      },
      "minSevenDayEpc": {
       "type": "number",
       "description": "Filters out all advertisers that have a seven day EPC average lower than the given value (inclusive). Min value: 0.0. Optional.",
       "format": "double",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "The value of 'nextPageToken' from the previous page. Optional.",
       "location": "query"
      },
      "relationshipStatus": {
       "type": "string",
       "description": "Filters out all advertisers for which do not have the given relationship status with the requesting publisher.",
       "enum": [
        "approved",
        "available",
        "deactivated",
        "declined",
        "pending"
       ],
       "enumDescriptions": [
        "An advertiser that has approved your application.",
        "An advertiser program that's accepting new publishers.",
        "Deactivated means either the advertiser has removed you from their program, or it could also mean that you chose to remove yourself from the advertiser's program.",
        "An advertiser that did not approve your application.",
        "An advertiser program that you've already applied to, but they haven't yet decided to approve or decline your application."
       ],
       "location": "query"
      },
      "role": {
       "type": "string",
       "description": "The role of the requester. Valid values: 'advertisers' or 'publishers'.",
       "required": true,
       "enum": [
        "advertisers",
        "publishers"
       ],
       "enumDescriptions": [
        "The requester is requesting as an advertiser.",
        "The requester is requesting as a publisher."
       ],
       "location": "path"
      },
      "roleId": {
       "type": "string",
       "description": "The ID of the requesting advertiser or publisher.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "role",
      "roleId"
     ],
     "response": {
      "$ref": "Advertisers"
     },
     "scopes": [
      "https://www.googleapis.com/auth/gan",
      "https://www.googleapis.com/auth/gan.readonly"
     ]
    }
   }
  },
  "ccOffers": {
   "methods": {
    "list": {
     "id": "gan.ccOffers.list",
     "path": "publishers/{publisher}/ccOffers",
     "httpMethod": "GET",
     "description": "Retrieves credit card offers for the given publisher.",
     "parameters": {
      "advertiser": {
       "type": "string",
       "description": "The advertiser ID of a card issuer whose offers to include. Optional, may be repeated.",
       "repeated": true,
       "location": "query"
      },
      "projection": {
       "type": "string",
       "description": "The set of fields to return.",
       "enum": [
        "full",
        "summary"
       ],
       "enumDescriptions": [
        "Include all offer fields. This is the default.",
        "Include only the basic fields needed to display an offer."
       ],
       "location": "query"
      },
      "publisher": {
       "type": "string",
       "description": "The ID of the publisher in question.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "publisher"
     ],
     "response": {
      "$ref": "CcOffers"
     },
     "scopes": [
      "https://www.googleapis.com/auth/gan",
      "https://www.googleapis.com/auth/gan.readonly"
     ]
    }
   }
  },
  "events": {
   "methods": {
    "list": {
     "id": "gan.events.list",
     "path": "{role}/{roleId}/events",
     "httpMethod": "GET",
     "description": "Retrieves event data for a given advertiser/publisher.",
     "parameters": {
      "advertiserId": {
       "type": "string",
       "description": "Caret(^) delimited list of advertiser IDs. Filters out all events that do not reference one of the given advertiser IDs. Only used when under publishers role. Optional.",
       "location": "query"
      },
      "chargeType": {
       "type": "string",
       "description": "Filters out all charge events that are not of the given charge type. Valid values: 'other', 'slotting_fee', 'monthly_minimum', 'tier_bonus', 'credit', 'debit'. Optional.",
       "enum": [
        "credit",
        "debit",
        "monthly_minimum",
        "other",
        "slotting_fee",
        "tier_bonus"
       ],
       "enumDescriptions": [
        "A credit increases the publisher's payout amount and decreases the advertiser's invoice amount.",
        "A debit reduces the publisher's payout and increases the advertiser's invoice amount.",
        "A payment made to Google by an advertiser as a minimum monthly network fee.",
        "Catch all. Default if unset",
        "A one time payment made from an advertiser to a publisher.",
        "A payment from an advertiser to a publisher for the publisher maintaining a high tier level"
       ],
       "location": "query"
      },
      "eventDateMax": {
       "type": "string",
       "description": "Filters out all events later than given date. Optional. Defaults to 24 hours after eventMin.",
       "location": "query"
      },
      "eventDateMin": {
       "type": "string",
       "description": "Filters out all events earlier than given date. Optional. Defaults to 24 hours from current date/time.",
       "location": "query"
      },
      "linkId": {
       "type": "string",
       "description": "Caret(^) delimited list of link IDs. Filters out all events that do not reference one of the given link IDs. Optional.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Max number of offers to return in this page. Optional. Defaults to 20.",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "memberId": {
       "type": "string",
       "description": "Caret(^) delimited list of member IDs. Filters out all events that do not reference one of the given member IDs. Optional.",
       "location": "query"
      },
      "modifyDateMax": {
       "type": "string",
       "description": "Filters out all events modified later than given date. Optional. Defaults to 24 hours after modifyDateMin, if modifyDateMin is explicitly set.",
       "location": "query"
      },
      "modifyDateMin": {
       "type": "string",
       "description": "Filters out all events modified earlier than given date. Optional. Defaults to 24 hours before the current modifyDateMax, if modifyDateMax is explicitly set.",
       "location": "query"
      },
      "orderId": {
       "type": "string",
       "description": "Caret(^) delimited list of order IDs. Filters out all events that do not reference one of the given order IDs. Optional.",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "The value of 'nextPageToken' from the previous page. Optional.",
       "location": "query"
      },
      "productCategory": {
       "type": "string",
       "description": "Caret(^) delimited list of product categories. Filters out all events that do not reference a product in one of the given product categories. Optional.",
       "location": "query"
      },
      "publisherId": {
       "type": "string",
       "description": "Caret(^) delimited list of publisher IDs. Filters out all events that do not reference one of the given publishers IDs. Only used when under advertiser role. Optional.",
       "location": "query"
      },
      "role": {
       "type": "string",
       "description": "The role of the requester. Valid values: 'advertisers' or 'publishers'.",
       "required": true,
       "enum": [
        "advertisers",
        "publishers"
       ],
       "enumDescriptions": [
        "The requester is requesting as an advertiser.",
        "The requester is requesting as a publisher."
       ],
       "location": "path"
      },
      "roleId": {
       "type": "string",
       "description": "The ID of the requesting advertiser or publisher.",
       "required": true,
       "location": "path"
      },
      "sku": {
       "type": "string",
       "description": "Caret(^) delimited list of SKUs. Filters out all events that do not reference one of the given SKU. Optional.",
       "location": "query"
      },
      "status": {
       "type": "string",
       "description": "Filters out all events that do not have the given status. Valid values: 'active', 'canceled'. Optional.",
       "enum": [
        "active",
        "canceled"
       ],
       "enumDescriptions": [
        "Event is currently active.",
        "Event is currently canceled."
       ],
       "location": "query"
      },
      "type": {
       "type": "string",
       "description": "Filters out all events that are not of the given type. Valid values: 'action', 'transaction', 'charge'. Optional.",
       "enum": [
        "action",
        "charge",
        "transaction"
       ],
       "enumDescriptions": [
        "The completion of an application, sign-up, or other process. For example, an action occurs if a user clicks an ad for a credit card and completes an application for that card.",
        "A charge event is typically a payment between an advertiser, publisher or Google.",
        "A conversion event, typically an e-commerce transaction. Some advertisers use a transaction to record other types of events, such as magazine subscriptions."
       ],
       "location": "query"
      }
     },
     "parameterOrder": [
      "role",
      "roleId"
     ],
     "response": {
      "$ref": "Events"
     },
     "scopes": [
      "https://www.googleapis.com/auth/gan",
      "https://www.googleapis.com/auth/gan.readonly"
     ]
    }
   }
  },
  "links": {
   "methods": {
    "get": {
     "id": "gan.links.get",
     "path": "{role}/{roleId}/link/{linkId}",
     "httpMethod": "GET",
     "description": "Retrieves data about a single link if the requesting advertiser/publisher has access to it. Advertisers can look up their own links. Publishers can look up visible links or links belonging to advertisers they are in a relationship with.",
     "parameters": {
      "linkId": {
       "type": "string",
       "description": "The ID of the link to look up.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "role": {
       "type": "string",
       "description": "The role of the requester. Valid values: 'advertisers' or 'publishers'.",
       "required": true,
       "enum": [
        "advertisers",
        "publishers"
       ],
       "enumDescriptions": [
        "The requester is requesting as an advertiser.",
        "The requester is requesting as a publisher."
       ],
       "location": "path"
      },
      "roleId": {
       "type": "string",
       "description": "The ID of the requesting advertiser or publisher.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "role",
      "roleId",
      "linkId"
     ],
     "response": {
      "$ref": "Link"
     },
     "scopes": [
      "https://www.googleapis.com/auth/gan",
      "https://www.googleapis.com/auth/gan.readonly"
     ]
    },
    "insert": {
     "id": "gan.links.insert",
     "path": "{role}/{roleId}/link",
     "httpMethod": "POST",
     "description": "Inserts a new link.",
     "parameters": {
      "role": {
       "type": "string",
       "description": "The role of the requester. Valid values: 'advertisers' or 'publishers'.",
       "required": true,
       "enum": [
        "advertisers",
        "publishers"
       ],
       "enumDescriptions": [
        "The requester is requesting as an advertiser.",
        "The requester is requesting as a publisher."
       ],
       "location": "path"
      },
      "roleId": {
       "type": "string",
       "description": "The ID of the requesting advertiser or publisher.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "role",
      "roleId"
     ],
     "request": {
      "$ref": "Link"
     },
     "response": {
      "$ref": "Link"
     },
     "scopes": [
      "https://www.googleapis.com/auth/gan"
     ]
    },
    "list": {
     "id": "gan.links.list",
     "path": "{role}/{roleId}/links",
     "httpMethod": "GET",
     "description": "Retrieves all links that match the query parameters.",
     "parameters": {
      "advertiserCategory": {
       "type": "string",
       "description": "The advertiser's primary vertical.",
       "enum": [
        "apparel_accessories",
        "appliances_electronics",
        "auto_dealer",
        "automotive",
        "babies_kids",
        "blogs_personal_sites",
        "books_magazines",
        "computers",
        "dating",
        "department_stores",
        "education",
        "employment",
        "financial_credit_cards",
        "financial_other",
        "flowers_gifts",
        "grocery",
        "health_beauty",
        "home_garden",
        "hosting_domain",
        "internet_providers",
        "legal",
        "media_entertainment",
        "medical",
        "movies_games",
        "music",
        "nonprofit",
        "office_supplies",
        "online_games",
        "outdoor",
        "pets",
        "real_estate",
        "restaurants",
        "sport_fitness",
        "telecom",
        "ticketing",
        "toys_hobbies",
        "travel",
        "utilities",
        "wholesale_relationship",
        "wine_spirits"
       ],
       "enumDescriptions": [
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        ""
       ],
       "repeated": true,
       "location": "query"
      },
      "advertiserId": {
       "type": "string",
       "description": "Limits the resulting links to the ones belonging to the listed advertisers.",
       "format": "int64",
       "repeated": true,
       "location": "query"
      },
      "assetSize": {
       "type": "string",
       "description": "The size of the given asset.",
       "repeated": true,
       "location": "query"
      },
      "authorship": {
       "type": "string",
       "description": "The role of the author of the link.",
       "enum": [
        "advertiser",
        "publisher"
       ],
       "enumDescriptions": [
        "",
        ""
       ],
       "location": "query"
      },
      "linkType": {
       "type": "string",
       "description": "The type of the link.",
       "enum": [
        "banner",
        "text"
       ],
       "enumDescriptions": [
        "",
        ""
       ],
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Max number of items to return in this page. Optional. Defaults to 20.",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "The value of 'nextPageToken' from the previous page. Optional.",
       "location": "query"
      },
      "promotionType": {
       "type": "string",
       "description": "The promotion type.",
       "enum": [
        "buy_get",
        "coupon",
        "free_gift",
        "free_gift_wrap",
        "free_shipping",
        "none",
        "ongoing",
        "percent_off",
        "price_cut",
        "product_promotion",
        "sale",
        "sweepstakes"
       ],
       "enumDescriptions": [
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        ""
       ],
       "repeated": true,
       "location": "query"
      },
      "relationshipStatus": {
       "type": "string",
       "description": "The status of the relationship.",
       "enum": [
        "approved",
        "available"
       ],
       "enumDescriptions": [
        "",
        ""
       ],
       "location": "query"
      },
      "role": {
       "type": "string",
       "description": "The role of the requester. Valid values: 'advertisers' or 'publishers'.",
       "required": true,
       "enum": [
        "advertisers",
        "publishers"
       ],
       "enumDescriptions": [
        "The requester is requesting as an advertiser.",
        "The requester is requesting as a publisher."
       ],
       "location": "path"
      },
      "roleId": {
       "type": "string",
       "description": "The ID of the requesting advertiser or publisher.",
       "required": true,
       "location": "path"
      },
      "startDateMax": {
       "type": "string",
       "description": "The end of the start date range.",
       "location": "query"
      },
      "startDateMin": {
       "type": "string",
       "description": "The beginning of the start date range.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "role",
      "roleId"
     ],
     "response": {
      "$ref": "Links"
     },
     "scopes": [
      "https://www.googleapis.com/auth/gan",
      "https://www.googleapis.com/auth/gan.readonly"
     ]
    }
   }
  },
  "publishers": {
   "methods": {
    "get": {
     "id": "gan.publishers.get",
     "path": "{role}/{roleId}/publisher",
     "httpMethod": "GET",
     "description": "Retrieves data about a single advertiser if that the requesting advertiser/publisher has access to it. Only advertisers can look up publishers. Publishers can request information about themselves by omitting the publisherId query parameter.",
     "parameters": {
      "publisherId": {
       "type": "string",
       "description": "The ID of the publisher to look up. Optional.",
       "location": "query"
      },
      "role": {
       "type": "string",
       "description": "The role of the requester. Valid values: 'advertisers' or 'publishers'.",
       "required": true,
       "enum": [
        "advertisers",
        "publishers"
       ],
       "enumDescriptions": [
        "The requester is requesting as an advertiser.",
        "The requester is requesting as a publisher."
       ],
       "location": "path"
      },
      "roleId": {
       "type": "string",
       "description": "The ID of the requesting advertiser or publisher.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "role",
      "roleId"
     ],
     "response": {
      "$ref": "Publisher"
     },
     "scopes": [
      "https://www.googleapis.com/auth/gan",
      "https://www.googleapis.com/auth/gan.readonly"
     ]
    },
    "list": {
     "id": "gan.publishers.list",
     "path": "{role}/{roleId}/publishers",
     "httpMethod": "GET",
     "description": "Retrieves data about all publishers that the requesting advertiser/publisher has access to.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Max number of items to return in this page. Optional. Defaults to 20.",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "minNinetyDayEpc": {
       "type": "number",
       "description": "Filters out all publishers that have a ninety day EPC average lower than the given value (inclusive). Min value: 0.0. Optional.",
       "format": "double",
       "location": "query"
      },
      "minPayoutRank": {
       "type": "integer",
       "description": "A value between 1 and 4, where 1 represents the quartile of publishers with the lowest ranks and 4 represents the quartile of publishers with the highest ranks. Filters out all publishers with a lower rank than the given quartile. For example if a 2 was given only publishers with a payout rank of 25 or higher would be included. Optional.",
       "format": "int32",
       "minimum": "1",
       "maximum": "4",
       "location": "query"
      },
      "minSevenDayEpc": {
       "type": "number",
       "description": "Filters out all publishers that have a seven day EPC average lower than the given value (inclusive). Min value 0.0. Optional.",
       "format": "double",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "The value of 'nextPageToken' from the previous page. Optional.",
       "location": "query"
      },
      "publisherCategory": {
       "type": "string",
       "description": "Caret(^) delimted list of publisher categories. Valid categories: (unclassified|community_and_content|shopping_and_promotion|loyalty_and_rewards|network|search_specialist|comparison_shopping|email). Filters out all publishers not in one of the given advertiser categories. Optional.",
       "location": "query"
      },
      "relationshipStatus": {
       "type": "string",
       "description": "Filters out all publishers for which do not have the given relationship status with the requesting publisher.",
       "enum": [
        "approved",
        "available",
        "deactivated",
        "declined",
        "pending"
       ],
       "enumDescriptions": [
        "Publishers you've approved to your program.",
        "Publishers available for you to recruit.",
        "A publisher that you terminated from your program. Publishers also have the ability to remove themselves from your program.",
        "A publisher that you did not approve to your program.",
        "Publishers that have applied to your program. We recommend reviewing and deciding on pending publishers on a weekly basis."
       ],
       "location": "query"
      },
      "role": {
       "type": "string",
       "description": "The role of the requester. Valid values: 'advertisers' or 'publishers'.",
       "required": true,
       "enum": [
        "advertisers",
        "publishers"
       ],
       "enumDescriptions": [
        "The requester is requesting as an advertiser.",
        "The requester is requesting as a publisher."
       ],
       "location": "path"
      },
      "roleId": {
       "type": "string",
       "description": "The ID of the requesting advertiser or publisher.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "role",
      "roleId"
     ],
     "response": {
      "$ref": "Publishers"
     },
     "scopes": [
      "https://www.googleapis.com/auth/gan",
      "https://www.googleapis.com/auth/gan.readonly"
     ]
    }
   }
  }
 }
}
