package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ClusterPrivilegeDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ClusterPrivilegeRepresentation.PATH)
public interface ClusterPrivilegeRepresentation extends RepresentationEntity<ClusterPrivilegeDto> {
	
	String PATH = "clusterprivilege";
	
}
