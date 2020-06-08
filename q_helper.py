def q_helper(func_success, func_no_result, func_error, q):
    content = {}
    while not q.empty():
        query, response = q.get()
        if response.status_code == 200:
            func_success(query, response, content)
        elif response.status_code == 422:
            func_no_result(query, response, content)
        else:
            func_error(query, response, content)
    return content
