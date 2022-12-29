import datetime
from ..module import Module
from ..interfaces import IInterpreter


class ISOWeekModule(Module):
    """Implements words to manipulate ISO Week information

    See https://en.wikipedia.org/wiki/ISO_week_date for more info
    """

    def __init__(self, interp: IInterpreter):
        super().__init__('isoweek', interp, ISOWEEK_FORTHIC)
        self.add_module_word('WEEK-NUM', self.word_WEEK_NUM)
        self.add_module_word('QUARTER-START', self.word_QUARTER_START)
        self.add_module_word('QUARTER-END', self.word_QUARTER_END)
        self.add_module_word('QUARTER/YEAR', self.word_QUARTER_slash_YEAR)

    # ( date -- num )
    def word_WEEK_NUM(self, interp: IInterpreter):
        date = interp.stack_pop()
        result = self.date_to_week_num(date)
        interp.stack_push(result)

    # ( date -- date )
    def word_QUARTER_START(self, interp: IInterpreter):
        date = interp.stack_pop()
        week_num = self.date_to_week_num(date)
        quarter_num = int((week_num - 1) / 13) + 1
        quarter_to_week_num = {
            1: 1,
            2: 14,
            3: 27,
            4: 40
        }
        day_of_week = self.get_day_of_week(date)
        start_week = quarter_to_week_num[quarter_num]
        delta_days = 7 * (week_num - start_week) + (day_of_week - 1)

        result = date - datetime.timedelta(delta_days)
        interp.stack_push(result)

    # ( date -- date )
    def word_QUARTER_END(self, interp: IInterpreter):
        date = interp.stack_pop()
        week_num = self.date_to_week_num(date)

        quarter_num = int((week_num - 1) / 13) + 1
        quarter_to_week_num = {
            1: 13,
            2: 26,
            3: 39,
            4: 52
        }
        day_of_week = self.get_day_of_week(date)
        end_week = quarter_to_week_num[quarter_num]
        if quarter_num == 4 and self.is_long_year(date.timetuple().tm_year):
            end_week = 53

        delta_days = 7 * (end_week - week_num) - (day_of_week - 1) + 6  # End of ISO Week is Sunday

        result = date + datetime.timedelta(delta_days)
        interp.stack_push(result)

    # ( date qtr_offset -- [qtr year] )
    def word_QUARTER_slash_YEAR(self, interp: IInterpreter):
        qtr_offset = interp.stack_pop()
        date = interp.stack_pop()

        week_num = self.date_to_week_num(date)
        if week_num >= 1 and week_num <= 13:
            quarter = 1
        elif week_num >= 14 and week_num <= 26:
            quarter = 2
        elif week_num >= 27 and week_num <= 39:
            quarter = 3
        else:
            quarter = 4

        res_quarter = ((quarter - 1) + qtr_offset) % 4 + 1
        res_date = date + datetime.timedelta(qtr_offset * 13 * 7)
        res_year = res_date.timetuple().tm_year
        interp.stack_push([res_quarter, res_year])

    # ----------------------------------------
    # Helpers
    def get_day_of_week(self, date):
        day_of_week = date.timetuple().tm_wday + 1  # ISO Week Monday is 1
        return day_of_week

    # If Jan 1 or Dec 31 are Thursdays, it's a long year
    def is_long_year(self, year):
        jan_1 = datetime.date(year, 1, 1)
        dec_31 = datetime.date(year, 12, 31)
        result = jan_1.timetuple().tm_wday == 4 or dec_31.timetuple().tm_wday == 4
        return result

    # See Algorithms section of https://en.wikipedia.org/wiki/ISO_week_date
    def date_to_week_num(self, date):
        year = date.timetuple().tm_year
        day_of_week = self.get_day_of_week(date)

        day_of_year = date.timetuple().tm_yday
        week_number = int((day_of_year - day_of_week + 10) / 7)

        # If week number is 53 and this isn't a long year, the date is in the first week of the next year
        if week_number == 53 and not self.is_long_year(year):
            week_number = 1

        # If week number is 0, the date is in the last week of the previous year
        if week_number == 0:
            if self.is_long_year(year - 1):
                week_number = 53
            else:
                week_number = 52
        return week_number


ISOWEEK_FORTHIC = ''
