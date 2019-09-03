(* ::Package:: *)

(*function to perform in the backend*)


queryFunc[q_String]:=Module[{search,api,import,link,request},
search=StringReplace[q,WhitespaceCharacter->"+"];
api="https://api.duckduckgo.com/?q="<>search<>"&format=json"(*grabs API directly*);
import=Import[api,"RawJSON"];

(*below cases test for empty meta fields, empty direct results, 
and if those tests pass then the direct result is extracted*)

link=Which[
import["meta"]===Null,CloudObject["redirectPage"](*redirects to cloud notebook to notify failed search*),
import["Results"]==={},First[import["RelatedTopics"]]["FirstURL"](*redirects to search results page for manual selection*),
True,First[import["Results"]]["FirstURL"](*extracts top-rated hit of direct search results*)
];
request=Delayed[HTTPRedirect[link]]
]


(*querying form*)


form=FormFunction[

"query"-><|
"Hint"->"What would you like to know?",
"Interpreter"->"String",
"Label"->None
|>,

queryFunc[#query]&,

AppearanceRules-><|
"Title"->"Custom Search Engine",
"Description"->"Use the DuckDuckGo Instant Answer API to get answers for your searches without always needing to click on a result.",
"SubmitLabel"->"Search"
|>
]


(*redirect page for failed search*)


redirectPage=ExportForm[TextCell["The search could not be completed.", "Title"],"CloudCDF"]


(*deployments to cloud*)


CloudDeploy[form,"searchForm",Permissions->"Private"]


CloudDeploy[redirectPage,"redirectPage",Permissions->"Public"]
