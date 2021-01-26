package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.enums.ParameterStyle;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameters;

import ci.gouv.dgbf.system.actor.server.representation.entities.RequestDispatchSlipDto;

@Path(RequestDispatchSlipRepresentation.PATH)
public interface RequestDispatchSlipRepresentation extends RepresentationEntity<RequestDispatchSlipDto> {

	@POST
	@Path(PATH_RECORD)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Enregistrer un bordereau")
	@Parameters(value = {
			@Parameter(name = "Demande",allowEmptyValue = false,description = "Demande",required = true,style = ParameterStyle.DEFAULT)
	})
	Response record(RequestDispatchSlipDto requestDispatchSlipDto);
	
	@POST
	@Path(PATH_SEND)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Transmettre un bordereau")
	@Parameters(value = {
			@Parameter(name = "Demande",allowEmptyValue = false,description = "Demande",required = true,style = ParameterStyle.DEFAULT)
	})
	Response send(RequestDispatchSlipDto requestDispatchSlipDto);
	
	@POST
	@Path(PATH_PROCESS)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Traiter un bordereau")
	@Parameters(value = {
			@Parameter(name = "Demande",allowEmptyValue = false,description = "Demande",required = true,style = ParameterStyle.DEFAULT)
	})
	Response process(RequestDispatchSlipDto requestDispatchSlipDto);
	
	String PATH = "bordereaudemande";
	String PATH_RECORD = "enregistrer";
	String PATH_SEND = "transmettre";
	String PATH_PROCESS = "traiter";
	
	static RequestDispatchSlipRepresentation getProxy() {
		return ProxyGetter.getInstance().get(RequestDispatchSlipRepresentation.class);
	}
}