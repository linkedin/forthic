from ..module import Module
from ..interfaces import IInterpreter


class WikiStatusModule(Module):
    """This defines words for creating status reports in Confluence using Jira data
    """
    def __init__(self, interp: IInterpreter):
        super().__init__('wiki-status', interp, FORTHIC)


FORTHIC = """
[ "risk_factor" "project_status" "label" "status" ] VARIABLES
: |w/STATUS            (status !) "'Status' REC@ status @ ==" SELECT;
: |w/RISK-FACTOR       (risk_factor !) "'Risk_Factor' REC@ risk_factor @ ==" SELECT;
: |w/LABEL             (label !) "'Labels' REC@ label @ SWAP IN" SELECT ;
: |w/out-RISK-FACTOR   (risk_factor !) "'Risk_Factor' REC@ risk_factor @ !=" SELECT;
: |w/DUE-DATE          "'Due Date' REC@ >DATE NONE !=" SELECT;
: |IN-PAST             "TODAY <" SELECT ;

["color"] VARIABLES
: COLOR-VALUES   [
   [ "red"   1 ]
   [ "yellow" 2 ]
   [ "green"  3 ]
] REC;

# Returns the color value for a given color
: COLOR>VALUE   COLOR-VALUES SWAP |LOWER REC@  100 DEFAULT;

: COLOR-TITLE    [ color @ COLOR>VALUE " - " color @ ] CONCAT;
: COLOR-LOZENGE   ( color ! )  [ [ "None" "--" ] ] REC color @ REC@
    [ "{status:colour=" color @ "|title=" COLOR-TITLE "}" ] CONCAT DEFAULT;

: STATUS>COLOR   [
   [ "Blocked"       "Red" ]
   [ "Resolved"      "Blue" ]
   [ "Closed"        "Blue" ]
] REC SWAP REC@ "Gray" DEFAULT ;

: WIKI-LI               "# " SWAP CONCAT ;   # ( str -- str )

: GREEN   "#00875A" ;
: YELLOW   "#FFAB00" ;
: RED     "#DE350B" ;
: BLUE    "#B3D4FF" ;
: GRAY    "gray" ;
: WHITE    "white" ;

: COLOR-BOX   {confluence COLOR-BOX};
: COLOR-RISK-BOXES   [
   [ "Red"          "RED COLOR-BOX" ]
   [ "Blocked"      "RED COLOR-BOX" ]

   [ "Yellow"       "YELLOW COLOR-BOX" ]
   [ "At-Risk"      "YELLOW COLOR-BOX" ]
   [ "Not on track" "YELLOW COLOR-BOX" ]

   [ "Green"        "GREEN COLOR-BOX" ]
   [ "On Track"     "GREEN COLOR-BOX" ]

   [ "Blue"         "BLUE COLOR-BOX" ]
   [ "Completed"    "BLUE COLOR-BOX" ]
   [ "Canceled"     "BLUE COLOR-BOX" ]

   [ "Gray"         "GRAY COLOR-BOX" ]
   [ "Light Gray"   "GRAY COLOR-BOX" ]
] REC;

: COLOR-RISK-BOX   COLOR-RISK-BOXES SWAP REC@  "WHITE COLOR-BOX" DEFAULT   INTERPRET ;   # ( color -- color_box )

[ "color_update" ] VARIABLES
: HOVER-COLOR   color_update @ 0 NTH ;
: HOVER-UPDATE  color_update @ 1 NTH ;
: HOVER-COLOR-BAR   ( color_update ! ) HOVER-COLOR COLOR-RISK-BOX HOVER-UPDATE "hover_text" <REC! confluence.RENDER ;


["start_date" "num_weeks"] VARIABLES
: PAST-DATES   (num_weeks ! start_date !) [ start_date @ "7 +DAYS" num_weeks @ 1 - <REPEAT ] |IN-PAST ;

["parent_tickets" "fchild_jql" "child_fields"] VARIABLES
: PARENT-KEYS           parent_tickets @ "'key' REC@" MAP;
: CHILD-JQL             fchild_jql @ { INTERPRET };
: PARENT-KEY>CHILDREN   CHILD-JQL child_fields @ jira.SEARCH;
: TICKETS-BY-PARENT   (child_fields ! fchild_jql ! parent_tickets !) PARENT-KEYS DUP "PARENT-KEY>CHILDREN" MAP ZIP REC;
# : TICKETS-BY-PARENT     PARENT-KEYS DUP "PARENT-KEY>CHILDREN" MAP ZIP REC;


# -- Details Table
: WIKI-LIST   "WIKI-LI" MAP  /N JOIN " " CONCAT; # (items -- wiki_list)

["as_of" "as_of_field" "as_of_ticket_key" "as_of_fields"] VARIABLES
: AS-OF-FIELDS!   as_of_fields !;  # (fields --)
: TICKET-CHANGELOG            as_of_ticket_key @ as_of_fields @ jira.CHANGELOG;
: AS-OF-IN-FUTURE?            as_of @ TODAY >=;

: FIELD-CACHE-KEY         [ as_of_ticket_key @ as_of_field @ as_of @ DATE>STR ] "_" JOIN ;
: CACHE/GET-FIELD         as_of @ TICKET-CHANGELOG as_of_field @ jira.FIELD-AS-OF DUP FIELD-CACHE-KEY cache.CACHE!;
: FIELD-AS-OF-FORTHIC
    [ [ TRUE "project @ as_of_field @ REC@" ]
      [ FALSE "FIELD-CACHE-KEY cache.CACHE@  'CACHE/GET-FIELD' *DEFAULT" ]
    ] REC AS-OF-IN-FUTURE? REC@ ;
: FIELD-AS-OF             (as_of_ticket_key ! as_of  !) FIELD-AS-OF-FORTHIC INTERPRET ;
: AS-OF-FIELD!   as_of_field !;   # (field -- )

[
    "PAST-DATES"
    "|w/STATUS"
    "|w/RISK-FACTOR"
    "|w/LABEL"
    "|w/out-RISK-FACTOR"
    "|w/DUE-DATE"
    "|IN-PAST"
    "TICKETS-BY-PARENT"
    "STATUS>COLOR"
    "AS-OF-FIELD!"
    "AS-OF-FIELDS!"
    "FIELD-AS-OF"
    "WIKI-LIST"
    "COLOR-LOZENGE"
    "HOVER-COLOR-BAR"
] EXPORT

"""
