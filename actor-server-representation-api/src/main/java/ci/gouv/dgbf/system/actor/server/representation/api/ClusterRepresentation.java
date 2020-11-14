package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ClusterDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ClusterRepresentation.PATH)
public interface ClusterRepresentation extends RepresentationEntity<ClusterDto> {
	
	String PATH = "cluster";
	
}
