package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.enums.ParameterIn;
import org.eclipse.microprofile.openapi.annotations.enums.ParameterStyle;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameters;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDto;

@Path(RequestRepresentation.PATH)
@Tag(name = RequestRepresentation.TAG)
public interface RequestRepresentation extends RepresentationEntity<RequestDto> {

	@GET
	@Path(PATH_GET_ONE_TO_BE_CREATED_BY_TYPE_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Instantier une demande par l'identifiant du type")
	@Parameters(value = {
			@Parameter(name = QUERY_PARAMETER_NAME_TYPE_IDENTIFIER,allowEmptyValue = false,description = "Identifiant du type de demande",example = "EREQUETE"
					,in = ParameterIn.QUERY,required = true,style = ParameterStyle.SIMPLE)
	})
	Response getOneToBeCreatedByTypeIdentifier(@QueryParam(QUERY_PARAMETER_NAME_TYPE_IDENTIFIER) String typeIdentifier);
	
	static RequestRepresentation getProxy() {
		return ProxyGetter.getInstance().get(RequestRepresentation.class);
	}
	
	String PATH = "demande";
	String PATH_GET_ONE_TO_BE_CREATED_BY_TYPE_IDENTIFIER = "getonetobecreatedbytypeidentifier";
	
	String TAG = "Demande";
	
	String QUERY_PARAMETER_NAME_TYPE_IDENTIFIER = "type_identifiant";
}