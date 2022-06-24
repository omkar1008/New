&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.URI.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.


/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



LOG-MANAGER:LOGFILE-NAME = "C:/OpenEdge/WRK/request-log1.txt".
LOG-MANAGER:LOGGING-LEVEL=5.


DEFINE VARIABLE hQuery      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO.
DEFINE VARIABLE iNumFields  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLoop       AS INTEGER   NO-UNDO.

DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRetOK      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE httCust     AS HANDLE    NO-UNDO.
DEFINE VARIABLE httCust1     AS HANDLE    NO-UNDO.

DEFINE TEMP-TABLE ttcustomer LIKE customer.
define temp-table ttOrder
  FIELD ordernum AS INTEGER
  FIELD orderdate AS CHARACTER
  FIELD shipdate AS CHARACTER 
  FIELD Carrier AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttOrder

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttorder.ordernum ttorder.orderdate ttorder.shipdate ttorder.carrier   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttOrder
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttOrder.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttOrder
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttOrder


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiCustNum btnSearch fiName fiAddress2 ~
fiState fiAddress1 fiCountry fiCity BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS fiCustNum fiName fiAddress2 fiState ~
fiAddress1 fiCountry fiCity 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSearch 
     LABEL "Search" 
     SIZE 17 BY 1.

DEFINE VARIABLE fiAddress1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address1" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fiAddress2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address2" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fiCity AS CHARACTER FORMAT "X(256)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fiCountry AS CHARACTER FORMAT "X(256)":U 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fiCustNum AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cust#" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fiName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fiState AS CHARACTER FORMAT "X(256)":U 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttOrder SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttorder.ordernum
ttorder.orderdate
ttorder.shipdate
ttorder.carrier
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 127 BY 12.14 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiCustNum AT ROW 2.19 COL 14 COLON-ALIGNED WIDGET-ID 18
     btnSearch AT ROW 2.19 COL 49 WIDGET-ID 2
     fiName AT ROW 3.86 COL 14 COLON-ALIGNED WIDGET-ID 26
     fiAddress2 AT ROW 5.48 COL 57 COLON-ALIGNED WIDGET-ID 22
     fiState AT ROW 5.48 COL 101 COLON-ALIGNED WIDGET-ID 28
     fiAddress1 AT ROW 5.52 COL 14 COLON-ALIGNED WIDGET-ID 24
     fiCountry AT ROW 6.95 COL 57 COLON-ALIGNED WIDGET-ID 30
     fiCity AT ROW 7.1 COL 13.6 COLON-ALIGNED WIDGET-ID 20
     BROWSE-2 AT ROW 9.1 COL 15 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 163.4 BY 26 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 26
         WIDTH              = 163.4
         MAX-HEIGHT         = 37.76
         MAX-WIDTH          = 307.2
         VIRTUAL-HEIGHT     = 37.76
         VIRTUAL-WIDTH      = 307.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 fiCity DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /*This event will close the window and terminate the procedure.*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch C-Win
ON CHOOSE OF btnSearch IN FRAME DEFAULT-FRAME /* Search */
DO:

DEFINE VARIABLE oClient         AS IHTTPClient    NO-UNDO.
DEFINE VARIABLE oURI            AS URI            NO-UNDO.
DEFINE VARIABLE oCredentials    AS Credentials    NO-UNDO.

DEFINE VARIABLE oRequest        AS IHttpRequest   NO-UNDO.
DEFINE VARIABLE oResponse       AS IHttpResponse  NO-UNDO.
DEFINE VARIABLE oCookies        AS Cookie         NO-UNDO.

DEFINE VARIABLE oJsonResponse   AS JsonObject     NO-UNDO.
DEFINE VARIABLE oCustomer       AS JsonObject     NO-UNDO.
DEFINE VARIABLE oOrders         AS JsonArray      NO-UNDO.
DEFINE VARIABLE oJsonParam      AS JsonObject     NO-UNDO.




oClient = ClientBuilder:Build():KeepCookies(CookieJarBuilder:Build():CookieJar):Client.

oURI = NEW URI('http', '192.168.2.11', 8810).

oURI:Path =  '/TrainingRestAPI/rest/TrainingRestAPIService/MySecondApi?CustNum=' + fiCustNum:SCREEN-VALUE.

//oCredentials =  NEW Credentials('Tomcat Application', 'tomcat', 'tomat').
                    
oRequest = RequestBuilder:Build('GET', oURI)
                         //:AddJsonData(oJsonParam1)
                         :ContentType('application/json')                         
                         //:UsingCredentials(oCredentials)
                         //:usingBasicAuthentication(oCredentials)
                         :acceptJson()
                         :REQUEST.

oResponse = ResponseBuilder:Build():Response.
oClient:execute(oRequest, oResponse).
//oResponse = ClientBuilder:Build():Client:Execute(oRequest).



oJsonResponse = CAST(oResponse:entity, JsonObject) NO-ERROR.


oCustomer = oJsonResponse:GetJsonObject("CustomerInfo").  
  
ASSIGN
  cSourceType = "JsonObject"   
  cReadMode   = "empty".
  
lRetOK = TEMP-TABLE ttCustomer:READ-JSON(cSourceType, oCustomer, cReadMode) NO-ERROR .

FIND FIRST ttCustomer NO-ERROR.
IF AVAILABLE ttCustomer THEN
DO:
    ASSIGN 
        fiCustNum:SCREEN-VALUE = string(ttCustomer.CustNum)
         fiName:SCREEN-VALUE = ttCustomer.Name
         fiAddress1:SCREEN-VALUE = ttCustomer.Address
         fiAddress2:SCREEN-VALUE = ttCustomer.Address2
         fiCity:SCREEN-VALUE = ttCustomer.City
         fiState:SCREEN-VALUE = ttCustomer.State
         fiCountry:SCREEN-VALUE = ttCustomer.Country.
   
    
END.

 
 oOrders = oJsonResponse:GetJsonArray("Orders").  
 
 ASSIGN
  cSourceType = "JsonArray"   
  cReadMode   = "empty".
  
lRetOK = TEMP-TABLE ttOrder:READ-JSON(cSourceType, oOrders, cReadMode) NO-ERROR.

ASSIGN hBuffer    = TEMP-TABLE ttCustomer:DEFAULT-BUFFER-HANDLE
       iNumFields = hBuffer:NUM-FIELDS.

       
OPEN QUERY browse-2 FOR EACH ttOrder.


                     
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiCustNum fiName fiAddress2 fiState fiAddress1 fiCountry fiCity 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiCustNum btnSearch fiName fiAddress2 fiState fiAddress1 fiCountry 
         fiCity BROWSE-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

