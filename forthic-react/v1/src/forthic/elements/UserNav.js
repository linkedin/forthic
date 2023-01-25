import { useState, useEffect, useRef } from 'react';

import { Typeahead, AsyncTypeahead } from 'react-bootstrap-typeahead';

// UserTypeahead: Allows user lookup
// props:
//    * interp: Interpreter
//    * typeaheadLabelKey:  The field of the search result records that's used to match typeahead input
//    * fsearch: Forthic that takes a string and returns an array of records and is executed when typeahead is active
//    * fselected: Forthic that is executed when a user is selected
//    * onSelect: Javascript callback when a user is selected
export function UserTypeahead(props) {
    const [searching, set_searching] = useState(false)
    const [search_results, set_search_results] = useState([])

    function handle_keypress(event) {
        if (event.code == "Escape") {
            props.onSelect(null)
        }
    }

    return (
    <AsyncTypeahead
        id="UserTypeahead"
        ref={props.ref}
        isLoading={searching}
        labelKey={props.typeaheadLabelKey}
        onChange={async (selected) => {
            const username = selected[0].username
            props.interp.stack_push(username)
            await props.interp.run(props.fselected)
            props.onSelect(username)
        }}
        onKeyDown={(event) => handle_keypress(event)}
        onSearch={async (query) => {
            set_searching(true)
            props.interp.stack_push(query)
            await props.interp.run(props.fsearch)
              .then(() => {
                  let records = props.interp.stack_pop()
                  set_search_results(records)
                  set_searching(false)
              })
            }
        }
        options={search_results} />
    )
}

// UserBreadcrumbNav: Shows a horizontal list of clickable user breadcrumbs
// props:
//    * interp: Interpreter
//    * mgr_chain: List of records representing the current management chain. Each record must have `username` and `fullname` fields
//    * direct_reports: Array of user records that are direct reports of the current breadcrumb user
//    * breadcrumbLabelKey:  The user field to display in the breadcrumb
//    * fselected: called when user makes selection from this control
export function UserBreadcrumbNav(props) {
    function get_breadcrumb_drilldown() {
        if (!props.direct_reports || props.direct_reports.length == 0) return

        return (<li className="breadcrumb-item">
            <Typeahead
                id="UserBreadcrumbNav"
                onChange={async (selected) => {
                    props.interp.stack_push(selected[0].username)
                    await props.interp.run(props.fselected)
                }}
                labelKey={props.breadcrumbLabelKey}
                options={props.direct_reports}
                size="sm"
            >
            </Typeahead>
        </li>)
    }

    function get_breadcrumb(user_record) {
        return (
            <li className="breadcrumb-item">
                <a
                    className='text-primary'
                    onClick={async () => {
                        props.interp.stack_push(user_record.username)
                        await props.interp.run(props.fselected)
                    }}
                >
                    {user_record[props.breadcrumbLabelKey]}
                </a>
            </li>
        )
    }

    if (!props.mgr_chain)   return
    return (
        <nav aria-label="breadcrumb">
            <ol className="breadcrumb justify-content-left">
                {props.mgr_chain.map(rec => get_breadcrumb(rec))}
                {get_breadcrumb_drilldown()}
            </ol>
        </nav>
    )
}


// UserNav: Combines UserTypeahead and UserBreadcrumbNav controls
// props:
//    * interp: Interpreter
//    * breadcrumbLabelKey:  The user field to display in the breadcrumb
//    * typeaheadLabelKey:  The field of the search result records that's used to match typeahead input
//    * fsearch: Forthic that takes a string and returns an array of records and is executed when typeahead is active
//    * fselected: Forthic that is executed when a user is selected
//    * mgr_chain: List of records representing the current management chain. Each record must have `username` and `fullname` fields
//    * direct_reports: Array of user records that are direct reports of the current breadcrumb user
export function UserNav(props) {
    const [use_typeahead, set_use_typeahead] = useState(false)
    const ref = useRef(null);

    useEffect( () => {
        if (use_typeahead) {
            ref.current.focus()
        }
    }, [use_typeahead]);

    function get_primary_control() {
        let search_button = <button type="button" className="btn" onClick={() => set_use_typeahead(true)}>&#x1F50D;</button>
        let breadcrumb_nav = UserBreadcrumbNav({...props})
        if (!use_typeahead)   return(
            <>
                {search_button}
                <div className='mt-3'>
                    {breadcrumb_nav}
                </div>
            </>
        )
    }

    function get_user_typeahead() {
        let cancel_button = <button type="button" className="btn" onClick={() => set_use_typeahead(false)}>&#10006;</button>
        let user_typeahead = UserTypeahead({...props, ref: ref, onSelect: () => set_use_typeahead(false)})
        if (use_typeahead)   return (
            <>
                {cancel_button}
                <div className='mt-3'>
                    {user_typeahead}
                </div>
            </>
        )
    }

    return (
        <div className='d-flex flex-row align-items-center'>
            {get_primary_control()}
            {get_user_typeahead()}
        </div>

    )
}