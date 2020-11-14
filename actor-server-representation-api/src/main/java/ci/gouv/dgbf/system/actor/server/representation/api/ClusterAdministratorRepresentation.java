package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ClusterAdministratorDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ClusterAdministratorRepresentation.PATH)
public interface ClusterAdministratorRepresentation extends RepresentationEntity<ClusterAdministratorDto> {
	
	String PATH = "clusteradministrator";
	
}
