class QHelper:
    def __init__(self, func_success, func_no_result, func_error, verbose=False):
        self.func_success = func_success
        self.func_no_result = func_no_result
        self.func_error = func_error
        self.verbose = verbose

    def __call__(self, q):
        content = {}
        while not q.empty():
            query, response = q.get()
            if response.status_code == 200:
                self.func_success(query, response, content)
            elif response.status_code == 422:
                self.func_no_result(query, response, content)
            else:
                self.func_error(query, response, content)
        if self.verbose:
            print(f"Fetched {len(content)} queries")
        return content


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
