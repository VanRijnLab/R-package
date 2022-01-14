## Description of dataset columns

| Column name  | Data | Extra information |
| ------------- | ------------- | ------------- |
| lessonTitle  | Name of the lesson  |  |
| lessonId  | Unique numerical identifier of the lesson  | |
| userId  | Unique numerical identifier of the user  |  |
| screenName  |screenName of user  | Field can be empty.  |
| email  | Email of user | Field can be empty.  |
| factText  | The  question text presented to the user  | There is no factText if the fact question is presented by another kind of stimuli only (for example, an image) However, a fact question can be presented by a combination of text and other stimuli.  |
| factAnswer  | The correct answer to the presented fact | In the case of multiple-choice questions the factAnswer will also contain the answer in text. |
| factId  | Unique numerical identifier of the fact | factId is always given, regardless of what kind of fact is presented (text, image, etc.)  |
| presentationStartTime  | The timestamp at which the fact is first presented  | The timestamp is presented as [unix time](https://www.unixtimestamp.com/). |
| presentationDuration  |The amount of time between when the fact is presented on screen and when the user submits their response in ms  | presentationDuration includes the time that a user spends typing a response.  |
| reactionTime  | The amount of time between when the fact is presented on screen to when the first keystroke/click took place  | In general the reactionTime is shorter than the presentationDuration, because it takes time for the user to type out their whole response. In the case of multiple-choice questions they are the same, because the user only has to click once in total to submit their response.  |
| sessionTime  | Total time of the session in ms | The total time is counted from the moment the session starts. |
| correct  | Boolean whether user answered question correctly  | Facts that are presented in study trials (where the user does not have to give a response) are also counted as correct. |
| givenResponse  | String representing which response the user gave  | In the case of multiple-choice the text of the selected answer is filled in here. In the case of study trials this field is left empty.  |
| alternatives | Array of alternative answer options  | This field is empty apart from multiple-choice questions. The alternative answers have a factId, answerText, correct and imageFileId. Correct here means whether chosing this answer would be the correct response for this fact question. |
| backSpaceUsed  | Boolean representing whether backspace was used  | The boolean is TRUE if the backspace was used at any place in the response. |
| backSpacedFirstLetter  | Boolean whether the backspace was used on the first letter  | This boolean is also true if several letters are typed and all of them deleted, including the first letter. |
| numberOfChoices  | Integer with how many choices the user for an answer | If the answer is not in a multiple-choice format this field will be 0. In the case of a study trial the field will also be 0.  |
| lookAheadTime  | The amount of time the algoritm looks ahead in ms  | Default set at 15.000 ms: The model determines the activation of all items 15 seconds in the future and if the item with the lowest activation has an activation value below the forgetThreshold, that item will be scheduled for presentation. If all predicted activations are above the forgetThreshold, the model will introduce a new item.|
| forgetThreshold  | Threshold below which the user forgets a fact | Default set at -0.8.  |
| maxAlpha  | Maximal rate of forgetting  | Default set at 0.5. |
| minAlpha | Minimal rate of forgetting  | Default set at 0.15.  |
| keyCode  |  ASCII numbers representing which keys were pressed on the keyboard when the user gave their answer  | Field is empty for multiple-choice questions or study trials (since no answer is typed).  |
| keyCharacter  | Keys that were pressed corresponding to the keycodes |  Field is empty for multiple-choice questions or study trials (since no answer is typed). |
| keyTime  | Timestamps at which individual keys where pressed in ms  | The timestamp is presented as [unix time](https://www.unixtimestamp.com/).  Field is empty for multiple-choice questions or study trials (since no answer is typed).  |
| model | String that denotes model used for lesson  | The SlimStampen model with study trials results in an empty field here. The SlimStampen model without study trials is denoted by the string "SpacingWithoutStudy". The Simple Flashcard model results in the string "FlashCardModel" (with model parameter: "groupSize"). |
| studyTrials | Boolean whether the lesson contain study trials  | Study trials are trials in which a new fact is presented with the answer already given. Users don't have to give a response during a study trial.  |
| sessionId  | A unique generated identifier of the session of this user and lesson | A new sessionId is generated when the user starts a lesson. If a user leave the lesson and re-starts it at another time, a new sessionId is generated. |
| aggregateResponse  | Boolean whether lesson was an aggregated lesson  | Aggregated lessons take the 20 most difficult questions of a group of lessons.  |

## Description of concepts

*encounter* =  The number of times this fact has been presented to the user

*activation* =   Activation level in memory of the fact

*alpha* =   Rate of forgetting for the fact

*decay* =   Decay parameter for the fact

*reading_time* =  Normalised time it took the user to read the fact
