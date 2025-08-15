module.exports={proxy:{target:'localhost:3001',proxyReq:[function(proxyReq,req){proxyReq.setHeader('host',req.headers.host);}]},files:['index.f3'],open:false};
