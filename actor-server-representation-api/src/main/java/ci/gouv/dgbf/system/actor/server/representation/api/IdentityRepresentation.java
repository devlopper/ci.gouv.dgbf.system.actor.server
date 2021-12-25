package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.entities.IdentityDto;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(IdentityRepresentation.PATH)
public interface IdentityRepresentation extends RepresentationEntity<IdentityDto> {

	String PATH = "identite";

	@GET
	@Path("crypter-email")
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	Response encryptElectroncicMailAddress(@QueryParam("email") String electronicMailAddress);
	
	@GET
	@Path("decrypter-email")
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	Response decryptElectroncicMailAddress(@QueryParam("email-crypte") String encryptElectronicMailAddress);
	
	static IdentityRepresentation getProxy() {
		return ProxyGetter.getInstance().get(IdentityRepresentation.class);
	}
}