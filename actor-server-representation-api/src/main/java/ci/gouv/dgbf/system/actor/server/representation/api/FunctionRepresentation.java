package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import ci.gouv.dgbf.system.actor.server.representation.entities.FunctionDto;
import io.swagger.annotations.ApiOperation;

import org.cyk.utility.server.representation.RepresentationEntity;

@Path(FunctionRepresentation.PATH)
public interface FunctionRepresentation extends RepresentationEntity<FunctionDto> {
	
	@POST
	@Path(PATH_IMPORT_FROM_KEYCLOAK)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Importer les fonctions à partir des roles de keycloak",tags = {TAG})
	Response importFromKeycloak();
	
	@POST
	@Path(PATH_EXPORT_TO_KEYCLOAK)
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Exporter les fonctions vers les roles de keycloak",tags = {TAG})
	Response exportToKeycloak();
	
	String PATH = "function";
	String PATH_IMPORT_FROM_KEYCLOAK = "importFromKeycloak";
	String PATH_EXPORT_TO_KEYCLOAK = "exportToKeycloak";
	
	String TAG = "Fonctions";
}