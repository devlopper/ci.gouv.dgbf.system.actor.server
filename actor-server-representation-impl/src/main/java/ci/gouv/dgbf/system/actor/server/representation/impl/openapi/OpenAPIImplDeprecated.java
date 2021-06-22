package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import javax.ws.rs.Path;

import org.eclipse.microprofile.openapi.annotations.servers.Server;

@Path(OpenAPIImplDeprecated.PATH)
@Server(url = "/api")
public class OpenAPIImplDeprecated extends AbstractOpenAPIImpl {

	public static final String PATH = "externe";
	
}