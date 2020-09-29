package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ServiceDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ServiceRepresentation.PATH)
public interface ServiceRepresentation extends RepresentationEntity<ServiceDto> {
	
	String PATH = "service";
	
}
