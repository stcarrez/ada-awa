# Questions Module

### Beans

| Name           | Description                                                               |
|:---------------|:--------------------------------------------------------------------------|
|question|This bean allows to create or edit a question.|
|answer|This bean allows to create or edit an answer.|
|questionList|This list bean represents the list of questions.|
|questionInfo|This list bean represents the list of questions.|
|questionVote|The vote bean that allows to vote for a question.|
|questionTagSearch|The question tag search bean.|
|questionTagCloud|The list of tags associated with question entities.|
|questionTags|The question tag editor bean.|
|answerVote|The vote bean that allows to vote for an answer.|

### Permissions

| Name           | Entity type  | Description                                                |
|:---------------|:-------------|:-----------------------------------------------------------|
|question-create|awa_workspace||
|question-edit|awa_question|Grant the edit permission to author only|
|question-delete|awa_question|Grant the delete permission to author only|
|answer-create|awa_question||
|answer-edit|awa_answer|Grant the edit permission to any user who is authenticated|
|answer-delete|awa_answer|Grant the delete permission to author only|
|answer-accept|awa_answer|The question author is the only one who can accept an answer.|

### Queries

| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|question-list|Get a list of questions.|
|question-tag-list|Get a list of questions filtered by a tag.|

### Mapping

#### AWA.Questions.Models.Question_Info

The list of questions.

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|id|the question identifier.|
||String|title|the question title.|
||Date|create_date|the question creation date.|
||String|description|the question short description.|
||Integer|rating|the question rating.|
||Integer|answer_count|the number of answers.|
||Identifier|author_id|the author's identifier.|
||String|author_name|the author's name.|
||String|author_email|the author's email.|

### Queries

| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|question-info|Get the detailed information for a question.|

### Mapping

#### AWA.Questions.Models.Question_Display_Info

The list of questions.

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|id|the question identifier.|
||String|title|the question title.|
||Date|create_date|the question creation date.|
||Nullable_Date|edit_date|the question edit date.|
||String|description|the question description.|
||Integer|rating|the question rating.|
||Integer|user_rating|the question rating as voted by the current user.|
||Identifier|author_id|the author's identifier.|
||String|author_name|the author's name.|
||String|author_email|the author's email.|

### Queries

| Name              | Description                                                           |
|:------------------|:----------------------------------------------------------------------|
|answer-list|Get a list of answers.|

### Mapping

#### AWA.Questions.Models.Answer_Info

The list of answers.

| Type     | Ada      | Name       | Description                                             |
|:---------|:---------|:-----------|:--------------------------------------------------------|
||Identifier|id|the answer identifier.|
||Date|create_date|the answer creation date.|
||Nullable_Date|edit_date|the answer edit date.|
||String|answer|the answer description.|
||Integer|rank|the answer rank.|
||Integer|user_rating|the question rating as voted by the current user.|
||Identifier|author_id|the author's identifier.|
||String|author_name|the author's name.|
||String|author_email|the author's email.|

## Model
![](images/awa_questions_model.png)


