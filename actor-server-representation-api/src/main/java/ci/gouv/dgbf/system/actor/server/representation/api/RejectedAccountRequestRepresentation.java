package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.RejectedAccountRequestDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(RejectedAccountRequestRepresentation.PATH)
public interface RejectedAccountRequestRepresentation extends RepresentationEntity<RejectedAccountRequestDto> {
	
	String PATH = "rejectedaccountrequest";
	
}
