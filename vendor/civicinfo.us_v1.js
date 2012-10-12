{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "civicinfo:us_v1",
 "name": "civicinfo",
 "canonicalName": "Civic Info",
 "version": "us_v1",
 "revision": "20120904",
 "title": "Google Civic Information API",
 "description": "An API for accessing civic information.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/search-16.gif",
  "x32": "http://www.google.com/images/icons/product/search-32.gif"
 },
 "documentationLink": "https://developers.google.com/civic-information",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/civicinfo/us_v1/",
 "basePath": "/civicinfo/us_v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "civicinfo/us_v1/",
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
  "AdministrationRegion": {
   "id": "AdministrationRegion",
   "type": "object",
   "description": "Describes information about a regional election administrative area.",
   "properties": {
    "electionAdministrationBody": {
     "$ref": "AdministrativeBody",
     "description": "The election administration body for this area."
    },
    "local_jurisdiction": {
     "$ref": "AdministrationRegion",
     "description": "The city or county that provides election information for this voter. This object can have the same elements as state."
    },
    "name": {
     "type": "string",
     "description": "The name of the jurisdiction."
    },
    "sources": {
     "type": "array",
     "description": "A list of sources for this area. If multiple sources are listed the data has been aggregated from those sources.",
     "items": {
      "$ref": "Source"
     }
    }
   }
  },
  "AdministrativeBody": {
   "id": "AdministrativeBody",
   "type": "object",
   "description": "Information about an election administrative body (e.g. County Board of Elections).",
   "properties": {
    "absenteeVotingInfoUrl": {
     "type": "string",
     "description": "A URL provided by this administrative body for information on absentee voting."
    },
    "ballotInfoUrl": {
     "type": "string",
     "description": "A URL provided by this administrative body to give contest information to the voter."
    },
    "correspondenceAddress": {
     "$ref": "SimpleAddressType",
     "description": "The mailing address of this administrative body."
    },
    "electionInfoUrl": {
     "type": "string",
     "description": "A URL provided by this administrative body for looking up general election information."
    },
    "electionOfficials": {
     "type": "array",
     "description": "The election officials for this election administrative body.",
     "items": {
      "$ref": "ElectionOfficial"
     }
    },
    "electionRegistrationConfirmationUrl": {
     "type": "string",
     "description": "A URL provided by this administrative body for confirming that the voter is registered to vote."
    },
    "electionRegistrationUrl": {
     "type": "string",
     "description": "A URL provided by this administrative body for looking up how to register to vote."
    },
    "electionRulesUrl": {
     "type": "string",
     "description": "A URL provided by this administrative body describing election rules to the voter."
    },
    "hoursOfOperation": {
     "type": "string",
     "description": "A description of the hours of operation for this administrative body."
    },
    "name": {
     "type": "string",
     "description": "The name of this election administrative body."
    },
    "physicalAddress": {
     "$ref": "SimpleAddressType",
     "description": "The physical address of this administrative body."
    },
    "voter_services": {
     "type": "array",
     "description": "A description of the services this administrative body may provide.",
     "items": {
      "type": "string"
     }
    },
    "votingLocationFinderUrl": {
     "type": "string",
     "description": "A URL provided by this administrative body for looking up where to vote."
    }
   }
  },
  "Candidate": {
   "id": "Candidate",
   "type": "object",
   "description": "Information about a candidate running for elected office.",
   "properties": {
    "candidateUrl": {
     "type": "string",
     "description": "The URL for the candidate's campaign web site."
    },
    "channels": {
     "type": "array",
     "description": "A list of known (social) media channels for this candidate.",
     "items": {
      "$ref": "Channel"
     }
    },
    "email": {
     "type": "string",
     "description": "The email address for the candidate's campaign."
    },
    "name": {
     "type": "string",
     "description": "The candidate's name."
    },
    "orderOnBallot": {
     "type": "string",
     "description": "The order the candidate appears on the ballot for this contest.",
     "format": "int64"
    },
    "party": {
     "type": "string",
     "description": "The full name of the party the candidate is a member of."
    },
    "phone": {
     "type": "string",
     "description": "The voice phone number for the candidate's campaign office."
    },
    "photoUrl": {
     "type": "string",
     "description": "A URL for a photo of the candidate."
    }
   }
  },
  "Channel": {
   "id": "Channel",
   "type": "object",
   "description": "A social media or web channel for a candidate.",
   "properties": {
    "id": {
     "type": "string",
     "description": "The unique public identifier for the candidate's channel."
    },
    "type": {
     "type": "string",
     "description": "The type of channel. The following is a list of types of channels, but is not exhaustive. More channel types may be added at a later time. One of: GooglePlus, YouTube, Facebook, Twitter"
    }
   }
  },
  "Contest": {
   "id": "Contest",
   "type": "object",
   "description": "Information about a contest that appears on a voter's ballot.",
   "properties": {
    "ballotPlacement": {
     "type": "string",
     "description": "A number specifying the position of this contest on the voter's ballot.",
     "format": "int64"
    },
    "candidates": {
     "type": "array",
     "description": "The candidate choices for this contest.",
     "items": {
      "$ref": "Candidate"
     }
    },
    "district": {
     "$ref": "ElectoralDistrict",
     "description": "Information about the electoral district that this contest is in."
    },
    "electorateSpecifications": {
     "type": "string",
     "description": "A description of any additional eligibility requirements for voting in this contest."
    },
    "level": {
     "type": "string",
     "description": "The level of office for this contest. One of: federal, state, county, city, other"
    },
    "numberElected": {
     "type": "string",
     "description": "The number of candidates that will be elected to office in this contest.",
     "format": "int64"
    },
    "numberVotingFor": {
     "type": "string",
     "description": "The number of candidates that a voter may vote for in this contest.",
     "format": "int64"
    },
    "office": {
     "type": "string",
     "description": "The name of the office for this contest."
    },
    "primaryParty": {
     "type": "string",
     "description": "If this is a partisan election, the name of the party it is for."
    },
    "sources": {
     "type": "array",
     "description": "A list of sources for this contest. If multiple sources are listed, the data has been aggregated from those sources.",
     "items": {
      "$ref": "Source"
     }
    },
    "special": {
     "type": "string",
     "description": "\"Yes\" or \"No\" depending on whether this a contest being held outside the normal election cycle."
    },
    "type": {
     "type": "string",
     "description": "The type of contest. Usually this will be General, Primary, or Run-off."
    }
   }
  },
  "Election": {
   "id": "Election",
   "type": "object",
   "description": "Information about the election that was queried.",
   "properties": {
    "electionDay": {
     "type": "string",
     "description": "Day of the election in YYYY-MM-DD format."
    },
    "id": {
     "type": "string",
     "description": "The unique ID of this election.",
     "format": "int64"
    },
    "name": {
     "type": "string",
     "description": "A displayable name for the election."
    }
   }
  },
  "ElectionOfficial": {
   "id": "ElectionOfficial",
   "type": "object",
   "description": "Information about individual election officials.",
   "properties": {
    "emailAddress": {
     "type": "string",
     "description": "The email address of the election official."
    },
    "faxNumber": {
     "type": "string",
     "description": "The fax number of the election official."
    },
    "name": {
     "type": "string",
     "description": "The full name of the election official."
    },
    "officePhoneNumber": {
     "type": "string",
     "description": "The office phone number of the election official."
    },
    "title": {
     "type": "string",
     "description": "The title of the election official."
    }
   }
  },
  "ElectionsQueryResponse": {
   "id": "ElectionsQueryResponse",
   "type": "object",
   "description": "The list of elections available for this version of the API.",
   "properties": {
    "elections": {
     "type": "array",
     "description": "A list of available elections",
     "items": {
      "$ref": "Election"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind, fixed to \"civicinfo#electionsQueryResponse\".",
     "default": "civicinfo#electionsQueryResponse"
    }
   }
  },
  "ElectoralDistrict": {
   "id": "ElectoralDistrict",
   "type": "object",
   "description": "Describes the geographic scope of a contest.",
   "properties": {
    "id": {
     "type": "string",
     "description": "An identifier for this district, relative to its scope. For example, the 34th State Senate district would have id \"34\" and a scope of stateUpper."
    },
    "name": {
     "type": "string",
     "description": "The name of the district."
    },
    "scope": {
     "type": "string",
     "description": "The geographic scope of this district. If unspecified the district's geography is not known. One of: statewide, congressional, stateUpper, stateLower, countywide, judicial, schoolBoard, cityWide, special"
    }
   }
  },
  "PollingLocation": {
   "id": "PollingLocation",
   "type": "object",
   "description": "A location where a voter can vote. This may be an early vote site or an election day voting location.",
   "properties": {
    "address": {
     "$ref": "SimpleAddressType",
     "description": "The address of the location"
    },
    "endDate": {
     "type": "string",
     "description": "The last date that this early vote site may be used. This field is not populated for polling locations."
    },
    "name": {
     "type": "string",
     "description": "The name of the early vote site. This field is not populated for polling locations."
    },
    "notes": {
     "type": "string",
     "description": "Notes about this location (e.g. accessibility ramp or entrance to use)"
    },
    "pollingHours": {
     "type": "string",
     "description": "A description of when this location is open."
    },
    "sources": {
     "type": "array",
     "description": "A list of sources for this location. If multiple sources are listed the data has been aggregated from those sources.",
     "items": {
      "$ref": "Source"
     }
    },
    "startDate": {
     "type": "string",
     "description": "The first date that this early vote site may be used. This field is not populated for polling locations."
    },
    "voterServices": {
     "type": "string",
     "description": "The services provided by this early vote site. This field is not populated for polling locations."
    }
   }
  },
  "SimpleAddressType": {
   "id": "SimpleAddressType",
   "type": "object",
   "description": "A simple representation of an address.",
   "properties": {
    "city": {
     "type": "string",
     "description": "The city or town for the address."
    },
    "line1": {
     "type": "string",
     "description": "The street name and number of this address."
    },
    "line2": {
     "type": "string",
     "description": "The second line the address, if needed."
    },
    "line3": {
     "type": "string",
     "description": "The third line of the address, if needed."
    },
    "locationName": {
     "type": "string",
     "description": "The name of the location."
    },
    "state": {
     "type": "string",
     "description": "The US two letter state abbreviation of the address."
    },
    "zip": {
     "type": "string",
     "description": "The US Postal Zip Code of the address."
    }
   }
  },
  "Source": {
   "id": "Source",
   "type": "object",
   "description": "Contains information about the data source for the element containing it.",
   "properties": {
    "name": {
     "type": "string",
     "description": "The name of the data source."
    },
    "official": {
     "type": "boolean",
     "description": "Whether this data comes from an official government source."
    }
   }
  },
  "VoterInfoRequest": {
   "id": "VoterInfoRequest",
   "type": "object",
   "description": "A request for information about a voter.",
   "properties": {
    "address": {
     "type": "string",
     "description": "The registered address of the voter to look up."
    }
   }
  },
  "VoterInfoResponse": {
   "id": "VoterInfoResponse",
   "type": "object",
   "description": "The result of a voter info lookup query.",
   "properties": {
    "contests": {
     "type": "array",
     "description": "Contests that will appear on the voter's ballot",
     "items": {
      "$ref": "Contest"
     }
    },
    "earlyVoteSites": {
     "type": "array",
     "description": "Locations where the voter is eligible to vote early, prior to election day",
     "items": {
      "$ref": "PollingLocation"
     }
    },
    "election": {
     "$ref": "Election",
     "description": "The election that was queried."
    },
    "kind": {
     "type": "string",
     "description": "The kind, fixed to \"civicinfo#voterInfoResponse\".",
     "default": "civicinfo#voterInfoResponse"
    },
    "normalizedInput": {
     "$ref": "SimpleAddressType",
     "description": "The normalized version of the requested address"
    },
    "pollingLocations": {
     "type": "array",
     "description": "Locations where the voter is eligible to vote on election day. For states with mail-in voting only, these locations will be nearby drop box locations. Drop box locations are free to the voter and may be used instead of placing the ballot in the mail.",
     "items": {
      "$ref": "PollingLocation"
     }
    },
    "state": {
     "type": "array",
     "description": "Local Election Information for the state that the voter votes in. For the US, there will only be one element in this array.",
     "items": {
      "$ref": "AdministrationRegion"
     }
    },
    "status": {
     "type": "string",
     "description": "The result of the request. One of: success, noStreetSegmentFound, addressUnparseable, noAddressParameter, multipleStreetSegmentsFound, electionOver, electionUnknown, internalLookupFailure"
    }
   }
  }
 },
 "resources": {
  "elections": {
   "methods": {
    "electionQuery": {
     "id": "civicinfo.elections.electionQuery",
     "path": "elections",
     "httpMethod": "GET",
     "description": "List of available elections to query.",
     "response": {
      "$ref": "ElectionsQueryResponse"
     }
    },
    "voterInfoQuery": {
     "id": "civicinfo.elections.voterInfoQuery",
     "path": "voterinfo/{electionId}/lookup",
     "httpMethod": "POST",
     "description": "Looks up information relevant to a voter based on the voter's registered address.",
     "parameters": {
      "electionId": {
       "type": "string",
       "description": "The unique ID of the election to look up. A list of election IDs can be obtained at.https://www.googleapis.com/civicinfo/{version}/elections",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "officialOnly": {
       "type": "boolean",
       "description": "If set to true, only data from official state sources will be returned.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "electionId"
     ],
     "request": {
      "$ref": "VoterInfoRequest"
     },
     "response": {
      "$ref": "VoterInfoResponse"
     }
    }
   }
  }
 }
}
