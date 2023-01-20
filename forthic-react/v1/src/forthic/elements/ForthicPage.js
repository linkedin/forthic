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

    // First time init: Run PAGE-CONTENT, ensure that qparams have defaults, and subscribe to PAGE-BROKER
    useEffect(() => {
        if (initialized)   return

        let page_broker;
        let subscription;
        setTimeout(async () => {
            let interp = await get_interp()

            await interp.run(`{${props.module_name} PAGE-DEFAULT-QPARAMS PAGE-DEFAULT-STATE PAGE-CONTENT }`)
            const _content = interp.stack_pop()
            const _page_state = interp.stack_pop()
            const default_qparams = interp.stack_pop()

            set_page_state(_page_state)
            set_content(ensure_array(_content))
            ensure_search_params(default_qparams, search_params, set_search_params)

            // Subscribe to message broker
            await interp.run(`{${props.module_name} PAGE-BROKER }`)
            page_broker = interp.stack_pop()
            subscription = page_broker.subscribe((new_state) => set_page_state(prevState => { return {...prevState, ...new_state} }))
        })
        set_initialized(true)

        return () => {
            if (subscription)   page_broker.unsubscribe(subscription)
        }
    }, [initialized, props.module_name, search_params, set_search_params]);

    // Update PAGE-DATA if search_params change
    useEffect(() => {
        setTimeout(async () => {
            let interp = await get_interp()
            await interp.run(`{${props.module_name} PAGE-DATA-UPDATE }`)
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
