package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ClusterPrivilegesDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ClusterPrivilegesRepresentation.PATH)
public interface ClusterPrivilegesRepresentation extends RepresentationEntity<ClusterPrivilegesDto> {
	
	String PATH = "clusterprivileges";
	
}
