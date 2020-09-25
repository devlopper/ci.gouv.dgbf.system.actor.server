package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActivityCategoryDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ActivityCategoryRepresentation.PATH)
public interface ActivityCategoryRepresentation extends RepresentationEntity<ActivityCategoryDto> {
	
	String PATH = "activitycategory";
	
}
