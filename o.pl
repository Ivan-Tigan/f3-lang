p(html, attribute, class).
p(html, attribute, href).
p(html, attribute, id).
p(html, attribute, type).
p(html, attribute, name).
p(html, attribute, value).
p(html, attribute, "hx-post").
p(html, attribute, "hx-trigger").
p(html, attribute, "hx-ext").
p(html, attribute, "ws-connect").
p(html, attribute, "ws-connected").
p(html, attribute, "ws-send").
p(html, attribute, "hx-vals").
p(html, attribute, "hx-headers").
p(html, attribute, "ws-after-message").
p(html, attribute, "ws-swap").
p(html, attribute, "hs-ws").
p(html, attribute, "hx-target").
p(html, attribute, src).
p(html, element, a).
p(html, element, main).
p(html, element, header).
p(html, element, footer).
p(html, element, section).
p(html, element, div).
p(html, element, form).
p(html, element, input).
p(html, element, button).
p(html, element, script).
p(html, element, li).
p(html, element, ul).
p(html, element, nav).
p(homePage, a, page).
p(homePage, path, "/users").
p(homePage, content, homePageContent).
p(homePageContent, a, main).
p(homePageContent, child, [homePageContent, inner]).
p([homePageContent, inner], a, section).
p(graph([p(Guid, a, inPOST), p(Guid, path, "/add_task"), p(Guid, data, p(X, user, UserId)), p(Guid, data, p(X, taskName, TaskName)), p(Guid, =, TaskId)]), =>, graph([p([db, "world"], insert, p(UserId, hasTask, TaskId)), p([db, "world"], insert, p(TaskId, name, TaskName))])).
p(graph([p(Guid, a, inPOST), p(Guid, path, "/delete_task"), p(Guid, data, p(X, task, TaskId))]), =>, graph([p([db, "world"], delete, p(Uid, hasTask, TaskId)), p([db, "world"], delete, p(TaskId, P, O))])).
p(graph([p(Guid, a, inPOST), p(Guid, path, "/set_task_name"), p(Guid, data, p(X, task, TaskId)), p(Guid, data, p(X, newName, NewName))]), =>, graph([p([db, "world"], delete, p(TaskId, name, OldName)), p([db, "world"], insert, p(TaskId, name, NewName))])).
p([db, "world"], hasGraph, p("user_1", a, user)).
p([db, "world"], hasGraph, p("user_1", name, "Bob")).
p([db, "world"], hasGraph, p("user_1", hasTask, "user_1/task_1")).
p([db, "world"], hasGraph, p("user_1", hasTask, "user_1/task_2")).
p([db, "world"], hasGraph, p("user_1", hasTask, "user_1/task_3")).
p([db, "world"], hasGraph, p("user_1/task_1", name, "Bob's first task")).
p([db, "world"], hasGraph, p("user_1/task_1", number, 1)).
p([db, "world"], hasGraph, p("user_1/task_1", timestamp, "2020-01-01")).
p([db, "world"], hasGraph, p("user_1/task_2", name, "Bob's second task")).
p([db, "world"], hasGraph, p("user_1/task_3", name, "Bob's third task")).
p(htmxScript, a, script).
p(htmxScript, src, "https://unpkg.com/htmx.org").
p(htmxJsonPostExtension, a, script).
p(htmxJsonPostExtension, src, "https://unpkg.com/htmx.org/dist/ext/json-enc.js").
p(htmxWSExtension, a, script).
p(htmxWSExtension, src, "https://unpkg.com/htmx.org@1.9.12/dist/ext/ws.js").
p(htmxWSJoin, a, script).
p(htmxWSJoin, text, "(() => { console.log('hello'); document.addEventListener('htmx:wsOpen', (ev) => { console.log('must send'); ev.detail.socketWrapper.send(JSON.stringify({type:'ping'}))} )})()").
p(htmxSessionHeaders, a, script).
p(htmxSessionHeaders, text, "(() => {htmx.config.headers = { 'X-Session-ID': Math.random().toString(36) }; })()").
p(graph([p(X, portal, Path), p(Path, sha256, ShaPath), p(["portal_", ShaPath], sconcat, PortalId)]), =>, graph([p(X, "hx-ext", "ws"), p(X, "hx-trigger", "load"), p(X, "ws-connect", Path), p(X, child, Path), p(Path, a, div), p(Path, id, PortalId), p([Path, portalPage], a, portalPage), p([Path, portalPage], path, Path), p([Path, portalPage], content, [Path, portalPage, content]), p([Path, portalPage, content], a, div), p([Path, portalPage, content], id, PortalId)])).
p([header], a, header).
p([header], child, [header, nav]).
p([header, nav], a, nav).
p([header, nav], child, [header, nav, ul1]).
p([header, nav], child, [header, nav, ul2]).
p([header, nav, ul1], a, ul).
p([header, nav, ul1], child, [header, nav, ul1, li1]).
p([header, nav, ul1, li1], a, li).
p([header, nav, ul1, li1], text, "Logo").
p([header, nav, ul2], a, ul).
p([header, nav, ul2], child, [header, nav, ul2, li1]).
p([header, nav, ul2, li1], a, li).
p([header, nav, ul2, li1], child, [header, nav, ul2, li1, a]).
p([header, nav, ul2, li1, a], a, a).
p([header, nav, ul2, li1, a], href, "/users").
p([header, nav, ul2, li1, a], text, "Users").
p(graph([p([db, "world"], hasGraph, p(UID, a, user)), p([db, "world"], hasGraph, p(UID, name, Name)), p(["/user/", UID], sconcat, Path), p(["/ws/user/", UID], sconcat, PortalPath)]), =>, graph([p([homePageContent, inner], child, [homePageContent, inner, Path]), p([homePageContent, inner, Path], a, a), p([homePageContent, inner, Path], id, "hello"), p([homePageContent, inner, Path], href, Path), p([homePageContent, inner, Path], text, Name), p([userpage, UID, Name], a, page), p([userpage, UID, Name], path, Path), p([userpage, UID, Name], content, [userpage, UID, Name, content]), p([userpage, UID, Name, content], a, main), p([userpage, UID, Name, content], class, "container"), p([userpage, UID, Name, content], child, [header]), p([userpage, UID, Name, content], child, [userpage, UID, Name, content, inner]), p([userpage, UID, Name, content], child, htmxScript), p([userpage, UID, Name, content], child, htmxJsonPostExtension), p([userpage, UID, Name, content], child, htmxWSExtension), p([userpage, UID, Name, content, inner], a, div), p([userpage, UID, Name, content, inner], class, "flex flex-col justify-start p-2"), p([userpage, UID, Name, content, inner], child, [[userpage, UID, Name, content, inner], tasks]), p([[userpage, UID, Name, content, inner], tasks], a, div), p([[userpage, UID, Name, content, inner], tasks], portal, PortalPath), p([userpage, UID, Name, content, inner], child, [[userpage, UID, Name, content, inner], insert]), p([[userpage, UID, Name, content, inner], insert], a, form), p([[userpage, UID, Name, content, inner], insert], "hx-post", "/add_task"), p([[userpage, UID, Name, content, inner], insert], "hx-trigger", "submit"), p([[userpage, UID, Name, content, inner], insert], "hx-ext", "json-enc"), p([[userpage, UID, Name, content, inner], insert], "hx-target", "#response-container"), p([[userpage, UID, Name, content, inner], insert], class, "flex flex-col justify-start p-2"), p([[userpage, UID, Name, content, inner], insert], child, [insert, user]), p([userpage, UID, Name, content, inner], child, [[userpage, UID, Name, content, inner], [insert, user]]), p([[userpage, UID, Name, content, inner], [insert, user]], a, input), p([[userpage, UID, Name, content, inner], [insert, user]], type, "hidden"), p([[userpage, UID, Name, content, inner], [insert, user]], name, "user"), p([[userpage, UID, Name, content, inner], [insert, user]], value, UID), p([[userpage, UID, Name, content, inner], insert], child, [insert, text]), p([userpage, UID, Name, content, inner], child, [[userpage, UID, Name, content, inner], [insert, text]]), p([[userpage, UID, Name, content, inner], [insert, text]], a, input), p([[userpage, UID, Name, content, inner], [insert, text]], type, "text"), p([[userpage, UID, Name, content, inner], [insert, text]], name, "taskName"), p([[userpage, UID, Name, content, inner], insert], child, [insert, button]), p([userpage, UID, Name, content, inner], child, [[userpage, UID, Name, content, inner], [insert, button]]), p([[userpage, UID, Name, content, inner], [insert, button]], a, button), p([[userpage, UID, Name, content, inner], [insert, button]], type, "submit"), p([[userpage, UID, Name, content, inner], [insert, button]], text, "Add task"), p([[userpage, UID, Name, content, inner], insert], child, [insert, response]), p([userpage, UID, Name, content, inner], child, [[userpage, UID, Name, content, inner], [insert, response]]), p([[userpage, UID, Name, content, inner], [insert, response]], a, div), p([[userpage, UID, Name, content, inner], [insert, response]], id, "response-container"), p([[userpage, UID, Name, content, inner], [insert, response]], child, [response, testing]), p([[userpage, UID, Name, content, inner], insert], child, [insert, [response, testing]]), p([userpage, UID, Name, content, inner], child, [[userpage, UID, Name, content, inner], [insert, [response, testing]]]), p([[userpage, UID, Name, content, inner], [insert, [response, testing]]], a, div), p([[userpage, UID, Name, content, inner], [insert, [response, testing]]], text, "testing")])).
p(graph([p([db, "world"], hasGraph, p(UID, a, user)), p([db, "world"], hasGraph, p(UID, name, Name)), p([db, "world"], hasGraph, p(UID, hasTask, T)), p([db, "world"], hasGraph, p(T, name, TaskName)), p(["/ws/user/", UID], sconcat, PortalPath)]), =>, graph([p([PortalPath, portalPage, content], child, [[PortalPath, portalPage, content], [task, T]]), p([[PortalPath, portalPage, content], [task, T]], a, div), p([[PortalPath, portalPage, content], [task, T]], class, "h-10 p-2"), p([[PortalPath, portalPage, content], [task, T]], text, TaskName)])).
p(graph([p(html, element, El), p(X, a, El), p([Attrs, graph([p(html, attribute, Attr), p(X, Attr, Value), p([" ", Attr, "='", Value, "' "], sconcat, Attrs)])], collect, AttributesList), p(AttributesList, sconcat, AttributesString)]), =>, graph([p(X, hasAttributesString, AttributesString)])).
p(graph([p(html, element, El), p(X, a, El), p(X, hasAttributesString, AttributesString), p(["<", El, " ", AttributesString, ">"], sconcat, LeftHTML), p(["</", El, ">"], sconcat, RightHTML)]), =>, graph([p(X, hasLeftRightHTML, [LeftHTML, RightHTML])])).
p(graph([p(html, element, El), p(X, a, El), p(X, text, T)]), =>, graph([p(X, innerHTML, T)])).
p(graph([p(html, element, El), p(X, a, El), p(system, not, p(X, text, SomeText)), p([T, graph([p(X, child, C), p(C, hasHTML, T)])], collect, ChildrenList), p(ChildrenList, sconcat, ChildrenString)]), =>, graph([p(X, innerHTML, ChildrenString)])).
p(graph([p(X, innerHTML, InnerHTML), p(X, hasLeftRightHTML, [OpenTag, CloseTag]), p([OpenTag, InnerHTML, CloseTag], sconcat, HTML)]), =>, graph([p(X, hasHTML, HTML)])).
p(graph([p(X, a, portalPage), p(X, path, P), p(X, content, C), p(C, hasHTML, HTML)]), =>, graph([p(res, is, [P, ->, HTML])])).
p(graph([p(X, a, page), p(X, path, P), p(X, content, C), p(C, hasHTML, HTML), p(["<head><link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\"></head><body>", HTML, "</body>"], sconcat, FullHTML)]), =>, graph([p(res, is, [P, ->, FullHTML])])).
p(system, runWebServer, 3000).
p(system, query, [X, graph([p(res, is, X)])]).
