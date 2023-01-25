import { Module, ModuleWord } from '../module';
import React from "react"
import { AreaChart, BarChart, LineChart, ComposedChart, PieChart, RadarChart, RadialBarChart, ScatterChart,
    FunnelChart, Treemap, Sankey,
    ResponsiveContainer, Legend, Tooltip, Cell, Text, Label, LabelList, Customized,
    Area, Bar, Line, Scatter, XAxis, YAxis, ZAxis, Brush, CartesianAxis, CartesianGrid,
    ReferenceLine, ReferenceDot, ReferenceArea, ErrorBar, Funnel,
    Pie, Radar, RadialBar, PolarAngleAxis, PolarGrid, PolarRadiusAxis,
    Cross, Curve, Dot, Polygon, Rectangle, Sector
} from "recharts"
import {format} from "date-fns"
import { render_content_array, ensure_array } from '../utils';

const TAG_TO_TYPE = {
    AreaChart,
    BarChart,
    LineChart,
    ComposedChart,
    PieChart,
    RadarChart,
    RadialBarChart,
    ScatterChart,
    FunnelChart,
    Treemap,
    Sankey,
    ResponsiveContainer,
    Legend,
    Tooltip,
    Cell,
    Text,
    Label,
    LabelList,
    Customized,
    Area,
    Bar,
    Line,
    Scatter,
    XAxis,
    YAxis,
    ZAxis,
    Brush,
    CartesianAxis,
    CartesianGrid,
    ReferenceLine,
    ReferenceDot,
    ReferenceArea,
    ErrorBar,
    Funnel,
    Pie,
    Radar,
    RadialBar,
    PolarAngleAxis,
    PolarGrid,
    PolarRadiusAxis,
    Cross,
    Curve,
    Dot,
    Polygon,
    Rectangle,
    Sector
}


class RechartsModule extends Module {
    constructor(interp) {
        super("recharts", interp);

        let self = this;
        this.add_exportable_word(new ModuleWord("ELEMENT", (interp) => this.word_ELEMENT(interp), self));
        this.add_exportable_word(new ModuleWord("LABEL-FUNC", (interp) => this.word_LABEL_FUNC(interp), self));
        this.add_exportable_word(new ModuleWord("DATE-FORMATTER", (interp) => this.word_DATE_FORMATTER(interp), self));
        this.add_exportable_word(new ModuleWord("NUMBER-FORMATTER", (interp) => this.word_NUMBER_FORMATTER(interp), self));
        this.add_exportable_word(new ModuleWord("TRUNCATE-FORMATTER", (interp) => this.word_TRUNCATE_FORMATTER(interp), self));
        this.add_exportable_word(new ModuleWord("CUMULATIVE-DIST>CHART-DATA", (interp) => this.word_CUMULATIVE_DIST_to_CHART_DATA(interp), self));
    }

    // (content props tag -- Element)
    word_ELEMENT(interp) {
        const tag = interp.stack_pop()
        function Result() {
            let content_array = ensure_array(Result.content);
            let rendered_content = render_content_array(content_array);

            let element_class = TAG_TO_TYPE[tag];
            if (!element_class)   throw `Unknown recharts element: ${tag}`

            let res = React.createElement(
              element_class,
              Result.props,
              ...rendered_content
            );
            return res;
          }
        interp.stack_push(Result)
    }

    // (field -- func)
    // Returns a function that takes an entry and return entry[field]
    // This is meant to be used as the label for something like a pie segment
    word_LABEL_FUNC(interp) {
        const field = interp.stack_pop()
        function result(entry) {
            return entry[field]
        }
        interp.stack_push(result)
    }

    // (format_string -- formatter_func)
    // See https://date-fns.org/docs/Getting-Started for format_string options
    word_DATE_FORMATTER(interp) {
        const format_string = interp.stack_pop()

        function result(date) {
            try {
                return format(new Date(date), format_string);

            } catch (error) {
                // console.error("DATE-FORMATTER error", error, date)
                return date
            }
        }

        interp.stack_push(result)
    }

    // ( num_digits suffix -- formatter_func)
    word_NUMBER_FORMATTER(interp) {
        let suffix = interp.stack_pop()
        const num_digits = interp.stack_pop()

        if (!suffix)   suffix = ""

        function result(number) {
            try {
                return number.toFixed(num_digits) + suffix

            } catch (error) {
                console.error("NUMBER-FORMATTER", error, number)
                return number
            }
        }

        interp.stack_push(result)
    }

    // ( max_length -- formatter_func)
    word_TRUNCATE_FORMATTER(interp) {
        const max_len = interp.stack_pop()

        function result(label) {
            let res = label
            try {
                let short_string = label.substring(0, max_len);
                res = short_string
                if (short_string.length < label.length)   res = short_string + "..."
            } catch (error) {
                console.error("TRUNCATE-FORMATTER", error, label)
                res = label
            }
            return res
        }

        interp.stack_push(result)
    }


    // ( cumulative_dist -- charts_data )
    // This conditions the output of CUMULATIVE-DIST so it can be rendered into a line chart
    word_CUMULATIVE_DIST_to_CHART_DATA(interp) {
        let cumulative_dist = interp.stack_pop()
        if (!cumulative_dist)   cumulative_dist = {}

        const groups = Object.keys(cumulative_dist)

        function build_datapoint(index, breakpoints, cumulative_dist) {
            const date_str = breakpoints[index]
            const date = new Date(date_str + " 10:00");
            const today = new Date()

            let result = { date: date.getTime() }

            // If date is in future, return null values for pct resolved
            if (date > today) return result;

            // Else, fill out percentages
            Object.keys(cumulative_dist).forEach(group => {
                result[group] = cumulative_dist[group].breakpoint_pcts[index]
            })
            return result
        }

        function condition_data(cumulative_dist) {
            if (groups.length == 0) return []

            const breakpoints = cumulative_dist[groups[0]].breakpoints

            let result = []
            for (let i = 0; i < breakpoints.length; i++) {
                result.push(build_datapoint(i, breakpoints, cumulative_dist))
            }
            return result
        }

        const result = condition_data(cumulative_dist)
        interp.stack_push(result)
    }
}

export default RechartsModule;
