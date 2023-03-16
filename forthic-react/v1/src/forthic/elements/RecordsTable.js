import {useState, useEffect} from "react"

function RecordsTable(props) {
    // Props:
    //    records:  A list of records to render
    //    column_info: A list of records describing each column. Each record has the following fields
    //        field: What field in each record to render
    //        label (optional): What label to use as the heading for the table
    //        is_header (optional): Boolean to indicate if the column is a header column for each row
    //        fsort (optional): Forthic used to return a value for sorting the column. If specifed, enables sort
    //        fclick (optional): Forthic executed on click. The Forthic should expect the record value associated with the cell
    //        fformat (optional): Forthic executed to format value
    //        fformat_rec (optional): Forthic executed to format record (this is used over `fformat`)
    //        className (optional): Classnames for column
    //    total_info (optional): Record with the following fields
    //        total_row_label (optional): Specifies label to use for total row. If set, computes total row. NOTE: The row totals are computed only for the visible records
    //        total_col_label (optional): Specifies label to use for total col. If set, computes total col
    //        col_fclick (optional): Forthic executed on total column cell click. The Forthic should expect an array record values associated with the row
    //        row_fclick (optional): Forthic executed on total row cell click. The Forthic should expect an array record values associated with the row
    //        grand_fclick (optional): Forthic executed on grand total cell click. The Forthic should expect an array record values associated with the row
    //        col_className (optional): Classnames for total column
    //    pagination_info (optional): Record with the following fields
    //        page_size: Number of records to show in each page. Setting this enables pagination
    //    wrapper_className (optional): Adds a className to the wrapper div
    //    header_className (optional): Adds a className to the th elements in the table header
    //    interp: Forthic interpreter

    const [sort_key, set_sort_key] = useState(null)
    const [sorted_records, set_sorted_records] = useState(props.records)
    const [page_offset, set_page_offset] = useState(0)
    const [page_records, set_page_records] = useState([])
    let header_className = props.header_className ? props.header_className : ""

    if (!props.records)       throw "RecordsTable needs a 'records' prop"
    if (!props.column_info)   throw "RecordsTable needs a 'column_info' prop"

    // Handle sort key changes
    useEffect( () => {
        set_page_offset(0)
        if (!sort_key) {
            set_sorted_records([...props.records])
            return
        }

        setTimeout(async () => {
            let interp = props.interp
            interp.stack_push([...props.records])
            await interp.run(`'''${sort_key.fsort}''' !COMPARATOR SORT`)
            let records = interp.stack_pop()
            if (sort_key.dir == "DESC")   records.reverse()
            set_sorted_records(records)
        })
    }, [sort_key, props.records]);

    // Handle pagination changes
    useEffect( () => {
        setTimeout(async () => {
            let interp = props.interp
            let new_page_records = sorted_records

            if (props.pagination_info) {
                if (props.pagination_info.page_size <= 0)   throw "RecordsTable pagination_info.page_size must be > 0"

                let record_offset = page_offset * props.pagination_info.page_size
                let end_offset = record_offset + props.pagination_info.page_size
                new_page_records = sorted_records.slice(record_offset, end_offset)
            }

            async function run_formatter(forthic) {
                await interp.run(forthic)
                let result = interp.stack_pop()
                if (result instanceof Function)   return result()
                else                              return result
            }

            // Compute formatted cell values
            let formatted_page_records = []
            for (const rec of new_page_records) {
                let formatted_rec = {...rec}
                for (const info of props.column_info) {
                    let formatted_value = rec[info.field]
                    if (info.fformat_rec) {
                        interp.stack_push(rec)
                        formatted_value = await run_formatter(info.fformat_rec)
                    }
                    else if (info.fformat) {
                        interp.stack_push(rec[info.field])
                        formatted_value = await run_formatter(info.fformat)
                    }
                    formatted_rec[get_value_field(info.field)] = formatted_value
                }
                formatted_page_records.push(formatted_rec)
            }

            set_page_records(formatted_page_records)
        })
    }, [sorted_records, page_offset]);

    function get_value_field(field) {
        return `${field}--value`
    }

    function col_head(col_info) {
        let res = col_info.label
        if (!res)   res = col_info.field

        function handleClick() {
            if (!col_info.fsort)   return

            if (!sort_key || sort_key.field != col_info.field) {
                set_sort_key({
                    field: col_info.field,
                    fsort: col_info.fsort,
                    dir: "ASC"
                })
            }
            else if (sort_key.dir == "ASC") {
                set_sort_key({...sort_key, dir: "DESC"})
            }
            else {
                set_sort_key(null)
            }
        }

        function sort_indicator() {
            if (sort_key && sort_key.field == col_info.field) {
                switch(sort_key.dir) {
                    case "ASC":
                        return <span>&uarr;</span>
                    case "DESC":
                        return <span>&darr;</span>
                    default:
                        return ''
                }
            }
        }

        function class_string() {
            let className = header_className
            if (col_info.className) {
                className = col_info.className
            }

            if (col_info.fsort)   return className + " clickable"
            else                  return className
        }
        return <th scope="col" className={class_string()} onClick={() => handleClick()}>{res}&nbsp;{sort_indicator()}</th>
    }

    function total_col_head() {
        if (!props.total_info || props.total_info.total_col_label == undefined)   return
        let className = "total-col " + header_className
        if (props.total_info.col_className) {
            className = className + " " + props.total_info.col_className
        }
        return <th className={className}>{props.total_info.total_col_label}</th>
    }

    function record_row_cell(col_info, rec) {
        let rawValue = rec[col_info.field]
        let value = rec[get_value_field(col_info.field)]

        let className = ""
        if (col_info.className) {
            className = col_info.className
        }

        let click_handler = () => {}

        if (col_info.fclick) {
            className += " clickable"
            click_handler = async (raw_value) => {
                props.interp.stack_push(raw_value)
                await props.interp.run(col_info.fclick)
            }
        }

        if (col_info.is_header) {
            return <th className={className} scope="row" onClick={() => click_handler(rawValue)}>{value}</th>
        }
        else {
            return <td className={className} onClick={() => click_handler(rawValue)}>{value}</td>
        }
    }

    function is_number(val) {
        return (val != '' && isNaN(val) == false)
    }

    function total_col_cell(rec) {
        if (!props.total_info || props.total_info.total_col_label == undefined)   return

        let className = 'total-col'
        if (props.total_info.col_className) {
            className = className + " " + props.total_info.col_className
        }
        let click_handler = () => {}
        if (props?.total_info?.col_fclick) {
            className = className + " " + "total-col clickable"
            click_handler = async (raw_value) => {
                props.interp.stack_push(raw_value)
                await props.interp.run(props.total_info.col_fclick)
            }
        }

        let sum = 0
        let raw_values = []
        for (const col_info of props.column_info) {
            raw_values.push(rec[col_info.field])
            let val = rec[get_value_field(col_info.field)]
            if (is_number(val))   sum += val
        }
        return <td className={className} onClick={() => click_handler(raw_values)}>{sum}</td>
    }


    function record_row(rec) {
        return <tr>{props.column_info.map(col_info => record_row_cell(col_info, rec))} {total_col_cell(rec)}</tr>
    }

    function total_row_cell(col_info) {
        let className = ""
        let click_handler = () => {}
        if (props?.total_info?.row_fclick) {
            className = "clickable"
            click_handler = async (raw_value) => {
                props.interp.stack_push(raw_value)
                await props.interp.run(props.total_info.row_fclick)
            }
        }
        if (col_info.className) {
            className = className + " " + col_info.className
        }

        let sum = 0
        let raw_values = []
        for (const rec of page_records) {
            raw_values.push(rec[col_info.field])
            let val = rec[get_value_field(col_info.field)]
            if (is_number(val))   sum += val
        }
        return <td className={className} onClick={() => click_handler(raw_values)}>{sum}</td>
    }

    function total_total_cell() {
        if (!props.total_info || props.total_info.total_col_label == undefined)   return

        let className = "total-col total-row"
        if (props.total_info.col_className) {
            className = className + " " + props.total_info.col_className
        }
        let click_handler = () => {}
        if (props?.total_info?.grand_fclick) {
            className = className + " " + "total-col total-row clickable"
            click_handler = async (raw_value) => {
                props.interp.stack_push(raw_value)
                await props.interp.run(props.total_info.grand_fclick)
            }
        }

        let sum = 0
        let raw_values = []
        for (const col_info of props.column_info) {
            for (const rec of page_records) {
                raw_values.push(rec[col_info.field])
                let val = rec[get_value_field(col_info.field)]
                if (is_number(val))   sum += val
            }
        }
        return <td className={className} onClick={() => click_handler(raw_values)}>{sum}</td>
    }

    function total_row() {
        if (!props.total_info || props.total_info.total_row_label == undefined)   return

        return (
            <tr className="total-row">
                <th scope="row">{props.total_info.total_row_label}</th>
                {props.column_info.slice(1).map(col_info => total_row_cell(col_info))}
                {total_total_cell()}
            </tr>
        )
    }

    function get_num_pages() {
        return Math.ceil(sorted_records.length / props.pagination_info.page_size)
    }

    function page_offsets() {
        const MAX_NUM_OFFSETS = 5

        let result = []
        let num_pages = get_num_pages()

        // Try to keep current offset in middle of displayed offsets
        let starting_offset = page_offset - (MAX_NUM_OFFSETS - 1)/2
        if (num_pages - 1 - starting_offset < MAX_NUM_OFFSETS)   starting_offset = num_pages - MAX_NUM_OFFSETS - 1
        if (starting_offset < 0)   starting_offset = 0

        for (let i=starting_offset; i < num_pages; i++) {
            result.push(i)
            if (result.length >= MAX_NUM_OFFSETS)   break
        }
        return result
    }

    function page_link(offset) {
        const active_class = (page_offset == offset) ? "active" : ""
        const className = `page-item ${active_class}`
        return (
        <li className={className}>
            <a className="page-link" onClick={() => set_page_offset(offset)}>{offset + 1}</a>
        </li>)
    }

    function pagination_controls() {
        if (!props.pagination_info) return

        const prev_disabled = (page_offset == 0) ? "disabled" : ""
        const prev_className = `page-item ${prev_disabled}`

        let num_pages = get_num_pages()
        const next_disabled = (page_offset == num_pages - 1) ? "disabled" : ""
        const next_className = `page-item ${next_disabled}`

        // Figure out which records are being displayed
        const start_num = page_offset * props.pagination_info.page_size + 1;
        const end_num = start_num + page_records.length - 1

        let controls = ""
        if (page_offsets().length > 1) {
            controls = (
                <nav aria-label="Page navigation example">
                    <ul className="pagination">
                        <li className={prev_className}>
                            <a className="page-link" aria-label="Previous" onClick={() => set_page_offset(page_offset - 1)}>
                                <span aria-hidden="true">&laquo;</span>
                            </a>
                        </li>
                        {page_offsets().map(offset => page_link(offset))}
                        <li className={next_className}>
                            <a className="page-link" aria-label="Next" onClick={() => set_page_offset(page_offset + 1)}>
                                <span aria-hidden="true">&raquo;</span>
                            </a>
                        </li>
                    </ul>
                </nav>

            )
        }

        return (
            <div className="d-flex justify-content-between">
                <p>Showing records {start_num}&#8211;{end_num} of {sorted_records.length}</p>
                {controls}
            </div>
        )
    }

    let wrapper_className = props.wrapper_className ? props.wrapper_className : ""
    let className = "table " + props.className

    // Return the table
    return (
        <>
        <div className={wrapper_className}>
        <table className={className}>
            <thead>
                <tr>
                    {props.column_info.map(col_info => col_head(col_info))}
                    {total_col_head()}
                </tr>
            </thead>

            <tbody>
                {page_records.map(rec => record_row(rec))}
                {total_row()}
            </tbody>
        </table>
        </div>
        {pagination_controls()}
        </>
    )
}

export default RecordsTable;
