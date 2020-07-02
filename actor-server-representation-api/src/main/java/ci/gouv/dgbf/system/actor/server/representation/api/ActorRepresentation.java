package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;
import io.swagger.annotations.ApiOperation;

@Path(ActorRepresentation.PATH)
public interface ActorRepresentation extends RepresentationEntity<ActorDto> {
	
	@GET
	@Path(PATH_GET_ALL_INFORMATIONS_BY_CODE)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Obtenir toutes les informations d'un compte utilisateur",tags = {"get"})
	Response getAllInformationsByCode(@QueryParam(QUERY_PARAMETER_NAME_USER_NAME)String code);
	
	String PATH = "actor";
	String PATH_GET_ALL_INFORMATIONS_BY_CODE = "getAllInformationsByCode";
	
	String QUERY_PARAMETER_NAME_USER_NAME = "nom_utilisateur";
}