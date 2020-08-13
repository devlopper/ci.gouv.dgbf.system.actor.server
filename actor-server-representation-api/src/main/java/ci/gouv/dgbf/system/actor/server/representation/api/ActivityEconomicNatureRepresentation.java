package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActivityEconomicNatureDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ActivityEconomicNatureRepresentation.PATH)
public interface ActivityEconomicNatureRepresentation extends RepresentationEntity<ActivityEconomicNatureDto> {
	
	String PATH = "activityeconomicnature";
	
}
