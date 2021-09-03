package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeTypeDto;

@Path(ScopeTypeRepresentation.PATH)
public interface ScopeTypeRepresentation extends RepresentationEntity<ScopeTypeDto> {

	String PATH_CREATE = "create";
	@POST
	@Path(PATH_CREATE)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.APPLICATION_JSON})
	Response create(@QueryParam(ScopeType.FIELD_CODE) String code,@QueryParam(ScopeType.FIELD_NAME) String name,@QueryParam(ScopeType.FIELD_ORDER_NUMBER) Byte orderNumber
			,@QueryParam(ScopeType.FIELD_REQUESTABLE) Boolean requestable,@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode);
	
	String PATH_UPDATE = "update";
	@POST
	@Path(PATH_UPDATE)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.APPLICATION_JSON})
	Response update(@QueryParam(ScopeType.FIELD_IDENTIFIER) String identifier,@QueryParam(ScopeType.FIELD_CODE) String code,@QueryParam(ScopeType.FIELD_NAME) String name
			,@QueryParam(ScopeType.FIELD_ORDER_NUMBER) Byte orderNumber,@QueryParam(ScopeType.FIELD_REQUESTABLE) Boolean requestable
			,@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode);
	
	String PATH_SAVE = "save";
	@POST
	@Path(PATH_SAVE)
	@Consumes({MediaType.APPLICATION_FORM_URLENCODED})
	@Produces({MediaType.APPLICATION_JSON})
	Response save(@QueryParam(ScopeType.FIELD_IDENTIFIER) String identifier,@QueryParam(ScopeType.FIELD_CODE) String code,@QueryParam(ScopeType.FIELD_NAME) String name
			,@QueryParam(ScopeType.FIELD_ORDER_NUMBER) Byte orderNumber,@QueryParam(ScopeType.FIELD_REQUESTABLE) Boolean requestable
			,@QueryParam(ActorRepresentation.PARAMETER_USER_NAME) String actorCode);
	
	String PATH = "scopetype";

	String TAG = ScopeRepresentation.TAG;

	String PARAMETER_REQUESTABLE = "demandable";
	String DESCRIPTION_REQUESTABLE = "Demandable";
	String EXAMPLE_REQUESTABLE = "true";
	
	static ScopeTypeRepresentation getProxy() {
		return ProxyGetter.getInstance().get(ScopeTypeRepresentation.class);
	}
}