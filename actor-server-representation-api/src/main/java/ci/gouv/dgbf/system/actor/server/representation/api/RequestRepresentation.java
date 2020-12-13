package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
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
	
	@POST
	@Path(PATH_INITIALIZE)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Initialiser une demande")
	@Parameters(value = {
			@Parameter(name = "Demande",allowEmptyValue = false,description = "Demande",required = true,style = ParameterStyle.DEFAULT)
	})
	Response initialize(RequestDto request);
	
	@POST
	@Path(PATH_RECORD)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Enregistrer une demande")
	@Parameters(value = {
			@Parameter(name = "Demande",allowEmptyValue = false,description = "Demande",required = true,style = ParameterStyle.DEFAULT)
	})
	Response record(RequestDto request);
	
	@POST
	@Path(PATH_SUBMIT_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Soumettre une demande")
	@Parameters(value = {
			@Parameter(name = "Identifiant de la demande",allowEmptyValue = false,description = "Identifiant de la demande à soumettre",required = true
					,example = "01",style = ParameterStyle.SIMPLE)
	})
	Response submitByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier);
	
	@POST
	@Path(PATH_ACCEPT_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Accepter une demande")
	@Parameters(value = {
			@Parameter(name = "Identifiant de la demande",allowEmptyValue = false,description = "Identifiant de la demande à accepter",required = true
					,example = "01",style = ParameterStyle.SIMPLE)
	})
	Response acceptByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier);
	
	@POST
	@Path(PATH_REJECT_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Accepter une demande")
	@Parameters(value = {
			@Parameter(name = "Identifiant",allowEmptyValue = false,description = "Identifiant de la demande",required = true
					,example = "01",style = ParameterStyle.SIMPLE)
			,@Parameter(name = "Motif de rejet",allowEmptyValue = false,description = "Motif de rejet de la demande",required = true
					,example = "Informations incorrectes",style = ParameterStyle.SIMPLE)
	})
	Response rejectByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier,@QueryParam(QUERY_PARAMETER_NAME_REJECTION_REASON) String rejectionReason);
	
	static RequestRepresentation getProxy() {
		return ProxyGetter.getInstance().get(RequestRepresentation.class);
	}
	
	String PATH = "demande";
	String PATH_GET_ONE_TO_BE_CREATED_BY_TYPE_IDENTIFIER = "getonetobecreatedbytypeidentifier";
	String PATH_INITIALIZE = "initialize";
	String PATH_RECORD = "record";
	String PATH_SUBMIT_BY_IDENTIFIER = "submitbyidentifier";
	String PATH_ACCEPT_BY_IDENTIFIER = "acceptbyidentifier";
	String PATH_REJECT_BY_IDENTIFIER = "rejectbyidentifier";
	
	String TAG = "Demande";
	
	String QUERY_PARAMETER_NAME_TYPE_IDENTIFIER = "type_identifiant";
	String QUERY_PARAMETER_NAME_REJECTION_REASON = "motif_rejet";
}