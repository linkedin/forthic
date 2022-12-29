import {useState, useEffect} from "react"
import { useLocation, useSearchParams } from "react-router-dom";
import { ensure_array, render_content_array, ensure_search_params } from '../utils';
import { get_interp } from "../..";

function ForthicPage(props) {
    // Props
    //    module_name:  Name of Forthic module for this page
    const location = useLocation()
    const [content, set_content] = useState([])
    const [search_params, set_search_params] = useSearchParams()
    const [page_data, set_page_data] = useState()
    const [page_state, set_page_state] = useState({})
    const [initialized, set_initialized] = useState(false)

    window.exceptionHandler = (error) => {
        set_page_state({...page_state, error})
        console.error(error)
    }

    // This word is injected into the module for the ForthicPage to allow Forthic code to update the page state
    // (page_state -- )
    function word_UPDATE_PAGE_STATE_bang(interp) {
        let _page_state = interp.stack_pop()
        set_page_state(_page_state)
    }

    // First time init: Run PAGE-CONTENT and ensure that qparams have defaults
    useEffect(() => {
        if (initialized)   return

        setTimeout(async () => {
            let interp = await get_interp()

            // Add UPDATE-PAGE-STATE! to the module
            let page_module = interp.cur_module().find_module(props.module_name)
            page_module.add_module_word("UPDATE-PAGE-STATE!", word_UPDATE_PAGE_STATE_bang)
            console.log(page_module)

            await interp.run(`{${props.module_name} DEFAULT-QPARAMS DEFAULT-PAGE-STATE PAGE-CONTENT }`)
            const _content = interp.stack_pop()
            const _page_state = interp.stack_pop()
            const default_qparams = interp.stack_pop()

            set_page_state(_page_state)
            set_content(ensure_array(_content))
            ensure_search_params(default_qparams, search_params, set_search_params)
        })
        set_initialized(true)
    }, [initialized, props.module_name, search_params, set_search_params]);

    // Update PAGE-DATA if search_params change
    useEffect(() => {
        setTimeout(async () => {
            let interp = await get_interp()
            await interp.run(`{${props.module_name} UPDATED-PAGE-DATA}`)
            const _page_data = interp.stack_pop()
            set_page_data(_page_data)
        })
    }, [location, props.module_name]);

    // Re-render app content if page_data or page_state changes
    useEffect(() => {
        setTimeout(async () => {
            let interp = await get_interp()

            // Set page data
            interp.stack_push(page_data)
            await interp.run(`{${props.module_name} PAGE-DATA!}`)

            // Set page state
            interp.stack_push(page_state)
            await interp.run(`{${props.module_name} PAGE-STATE!}`)

            // Render page content
            await interp.run(`{${props.module_name} PAGE-CONTENT}`)
            const _content = interp.stack_pop()
            set_content([_content])
        })
    }, [search_params, page_data, page_state, props.module_name]);


    // Return rendered content
    return <>{render_content_array(content)}</>;
}

export default ForthicPage;
