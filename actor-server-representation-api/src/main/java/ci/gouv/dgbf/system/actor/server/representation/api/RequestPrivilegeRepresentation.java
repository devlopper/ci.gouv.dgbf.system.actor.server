package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestPrivilegeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(RequestPrivilegeRepresentation.PATH)
public interface RequestPrivilegeRepresentation extends RepresentationEntity<RequestPrivilegeDto> {
	
	String PATH = "requestprivilege";
	
}
