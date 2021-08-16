from forthic.interpreter import Interpreter
import forthic.modules.gsheet_module as gsheet_module
import forthic.modules.excel_module as excel_module

from .utils import creds


def get_interp():
    interp = Interpreter()
    interp.dev_mode = True

    def configure_gsheet_module(interp):
        interp.register_module(gsheet_module.GsheetModule)

        class GSheetCredsContext(gsheet_module.CredsContext):
            def get_app_creds(self):
                return creds.get_creds('GOOGLE_APP')

            def get_auth_token(self):
                return creds.get_creds('GOOGLE_TOKEN')

        interp.run("['gsheet'] USE-MODULES")
        interp.stack_push(GSheetCredsContext())
        interp.run('gsheet.PUSH-CONTEXT!')

    def configure_excel_module(interp):
        interp.register_module(excel_module.ExcelModule)

        class ExcelCredsContext(excel_module.CredsContext):
            def get_app_creds(self):
                return creds.get_creds('MSGRAPH_APP')

            def get_auth_token(self):
                return creds.get_creds('MSGRAPH_TOKEN')

        interp.run("['excel'] USE-MODULES")
        interp.stack_push(ExcelCredsContext())
        interp.run('excel.PUSH-CONTEXT!')

    configure_gsheet_module(interp)
    configure_excel_module(interp)
    return interp


def main():
    interp = get_interp()

    interp.run(
        '''
    # 'a-gsheet-id' 'Sheet1' gsheet.ROWS .s
    # 'a-gsheet-id' 'Sheet1' ['Greek' 'Col23'] gsheet.RECORDS .s
    # 'a-gsheet-id' 'Sheet1!D1:F2' [["Col1" "Col2" "Col3"] ["1" "2" 30]] gsheet.ROWS! .s
    # 'https://docs.google.com/spreadsheets/d/xxxxx/edit#gid=0' gsheet.URL>SHEET-ID/RANGE .s
    # 'https://docs.google.com/spreadsheets/d/xxxxx/edit#gid=0' gsheet.URL>SHEET-ID/RANGE gsheet.ROWS .s

    ["workbook_info"] VARIABLES
    "https://microsoft-my.sharepoint.com/..." excel.WORKBOOK-INFO workbook_info !

    # workbook_info @ .s excel.SHEET-NAMES .s
    # workbook_info @ "ENG" excel.TABLE-NAMES .s

    workbook_info @ "ENG" "ENG_HC" excel.TABLE-RECORDS .s

    # : NEW-ROWS   [
    #      [301 "Ice Cream" "Lunch"]
    #      [302 "Cookies" "Dinner"]
    # ] ;
    # workbook_id @ "Main" "Table1" NEW-ROWS excel.ADD-TABLE-ROWS

    # : UPDATED-VALUES   [[102 "Mark2.3" "Third version minor 3"]];
    # workbook_id @ "Main" "Main!F3:H3" UPDATED-VALUES excel.UPDATE-RANGE

    '''
    )


if __name__ == '__main__':
    main()
