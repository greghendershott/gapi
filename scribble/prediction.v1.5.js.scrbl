#lang scribble/manual
@title{Prediction API v1.5}
Lets you access a cloud hosted machine learning service that makes it easy to build smart apps
@hyperlink["https://developers.google.com/prediction/docs/developer-guide" "Documentation link"]
@table-of-contents{}
@section{API Parameters}
These optional keyword arguments may be passed to all functions for this API:
@defproc[(any-function
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
This is not actually a function. This is just using Scribble's
defproc form to list the optional keyword arguments that may be passed
to any function for this API.

@racket[fields]: Selector specifying which fields to include in a partial response.

@racket[key]: API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.

@racket[alt]: Data format for the response.

@racket[oauth_token]: OAuth 2.0 token for the current user.

@racket[prettyPrint]: Returns response with indentations and line breaks.

@racket[quotaUser]: Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters. Overrides userIp if both are provided.

@racket[userIp]: IP address of the site where the request originates. Use this if you want to enforce per-user limits.

}


@section{Functions for the `hostedmodels' resource}
@defproc[(prediction.hostedmodels.predict
[hostedModelName string?]
[#:input input string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Submit input and request an output against a hosted model.

@racket[hostedModelName]: The name of a hosted model.

@racket[input]: Input to the model for a prediction

}

@section{Functions for the `trainedmodels' resource}
@defproc[(prediction.trainedmodels.list
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List available models.

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: Pagination token

}

@defproc[(prediction.trainedmodels.get
[id string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Check training status of your model.

@racket[id]: The unique name for the predictive model.

}

@defproc[(prediction.trainedmodels.insert
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:created created string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:modelInfo modelInfo string? 'N/A]
[#:storageDataLocation storageDataLocation string? 'N/A]
[#:storagePMMLLocation storagePMMLLocation string? 'N/A]
[#:storagePMMLModelLocation storagePMMLModelLocation string? 'N/A]
[#:trainingComplete trainingComplete string? 'N/A]
[#:trainingStatus trainingStatus string? 'N/A]
[#:utility utility string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Begin training your model.

@racket[id]: The unique name for the predictive model.

@racket[kind]: What kind of resource this is.

@racket[created]: Insert time of the model (as a RFC 3339 timestamp).

@racket[selfLink]: A URL to re-request this resource.

@racket[modelInfo]: Model metadata.

@racket[storageDataLocation]: Google storage location of the training data file.

@racket[storagePMMLLocation]: Google storage location of the preprocessing pmml file.

@racket[storagePMMLModelLocation]: Google storage location of the pmml model file.

@racket[trainingComplete]: Training completion time (as a RFC 3339 timestamp).

@racket[trainingStatus]: The current status of the training job. This can be one of following: RUNNING; DONE; ERROR; ERROR: TRAINING JOB NOT FOUND

@racket[utility]: A class weighting function, which allows the importance weights for class labels to be specified [Categorical models only].

}

@defproc[(prediction.trainedmodels.predict
[id string?]
[#:input input string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Submit model id and request a prediction.

@racket[id]: The unique name for the predictive model.

@racket[input]: Input to the model for a prediction

}

@defproc[(prediction.trainedmodels.analyze
[id string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get analysis of the model and the data the model was trained on.

@racket[id]: The unique name for the predictive model.

}

@defproc[(prediction.trainedmodels.update
[id string?]
[#:label label string? 'N/A]
[#:csvInstance csvInstance string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Add new data to a trained model.

@racket[id]: The unique name for the predictive model.

@racket[label]: The class label of this instance

@racket[csvInstance]: The input features for this instance

}

@defproc[(prediction.trainedmodels.delete
[id string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Delete a trained model.

@racket[id]: The unique name for the predictive model.

}

