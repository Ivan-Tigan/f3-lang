module.exports={proxy:{target:'localhost:3001',proxyReq:[function(proxyReq,req){proxyReq.setHeader('host',req.headers.host);}]},files:['book.f3'],open:false};
