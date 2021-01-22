package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.List;

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

import ci.gouv.dgbf.system.actor.server.business.api.RequestBusiness;
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
	@Path(PATH_RECORD_PHOTO)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	Response recordPhoto(RequestDto request);
	
	@POST
	@Path(PATH_RECORD_PHOTO_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	Response recordPhotoByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier,byte[] bytes);
	
	@POST
	@Path(PATH_RECORD_ACT_OF_APPOINTMENT)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	Response recordActOfAppointment(RequestDto request);
	
	@POST
	@Path(PATH_RECORD_ACT_OF_APPOINTMENT_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	Response recordActOfAppointmentByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier,byte[] bytes);
	
	@POST
	@Path(PATH_RECORD_SIGNATURE)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	Response recordSignature(RequestDto request);
	
	@POST
	@Path(PATH_RECORD_SIGNATURE_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	Response recordSignatureByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier,byte[] bytes);
	
	@POST
	@Path(PATH_RECORD_SIGNED_REQUEST_SHEET)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	Response recordSignedRequestSheet(RequestDto request);
	
	@POST
	@Path(PATH_RECORD_SIGNED_REQUEST_SHEET_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	Response recordSignedRequestSheetByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier,byte[] bytes);
	
	@POST
	@Path(PATH_RECORD_SIGNED_REQUEST_SHEET_BY_IDENTIFIER_FOR_ADMIN)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	Response recordSignedRequestSheetByIdentifierForAdmin(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier,byte[] bytes);
	
	@POST
	@Path(PATH_SUBMIT_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Soumettre une demande")
	@Parameters(value = {
			@Parameter(name = "Identifiant de la demande",allowEmptyValue = false,description = "Identifiant de la demande à soumettre",required = true
					,example = "01",style = ParameterStyle.SIMPLE)
	})
	Response submitByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier,@QueryParam(QUERY_PARAMETER_NAME_READ_PAGE_URL) String readPageURL);
	
	@POST
	@Path(PATH_ACCEPT_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Accepter une demande")
	@Parameters(value = {
			@Parameter(name = "Identifiant de la demande",allowEmptyValue = false,description = "Identifiant de la demande à accepter",required = true
					,example = "01",style = ParameterStyle.SIMPLE)
	})
	Response acceptByIdentifier(
			@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier
			,@QueryParam(QUERY_PARAMETER_NAME_BUDGETAIRES_SCOPE_FUNCTIONS_IDENTIFIERS)List<String> budgetariesScopeFunctionsIdentifiers
			,@QueryParam(QUERY_PARAMETER_NAME_COMMENT) String comment			
			,@QueryParam(QUERY_PARAMETER_NAME_READ_PAGE_URL) String readPageURL
			,@QueryParam(QUERY_PARAMETER_NAME_ACTOR) String actorCode);
	
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
	Response rejectByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier,@QueryParam(QUERY_PARAMETER_NAME_REJECTION_REASON) String rejectionReason
			,@QueryParam(QUERY_PARAMETER_NAME_READ_PAGE_URL) String readPageURL,@QueryParam(QUERY_PARAMETER_NAME_ACTOR) String actorCode);
	
	@GET
	@Path(PATH_GET_PHOTO_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir la photo d'une demande")
	@Parameters(value = {
			@Parameter(name = "Identifiant de la demande",allowEmptyValue = false,description = "Identifiant de la demande",required = true
					,example = "01",style = ParameterStyle.SIMPLE)
	})
	Response getPhotoByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier);
	
	@GET
	@Path(PATH_GET_ACT_OF_APPOINTMENT_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir l'acte de nomination d'une demande")
	@Parameters(value = {
			@Parameter(name = "Identifiant de la demande",allowEmptyValue = false,description = "Identifiant de la demande",required = true
					,example = "01",style = ParameterStyle.SIMPLE)
	})
	Response getActOfAppointmentByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier);
	
	@GET
	@Path(PATH_GET_SIGNATURE_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir la signature d'une demande")
	@Parameters(value = {
			@Parameter(name = "Identifiant de la demande",allowEmptyValue = false,description = "Identifiant de la demande",required = true
					,example = "01",style = ParameterStyle.SIMPLE)
	})
	Response getSignatureByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier);
	
	@GET
	@Path(PATH_GET_SIGNED_REQUEST_SHEET_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Obtenir la fiche de demande signée d'une demande")
	@Parameters(value = {
			@Parameter(name = "Identifiant de la demande",allowEmptyValue = false,description = "Identifiant de la demande",required = true
					,example = "01",style = ParameterStyle.SIMPLE)
	})
	Response getSignedRequestSheetByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier);
	
	@GET
	@Path(PATH_BUILD_REPORT_BY_IDENTIFIER)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Construire l'état d'une demande")
	@Parameters(value = {
			@Parameter(name = "Identifiant de la demande",allowEmptyValue = false,description = "Identifiant de la demande pour construire l'état",required = true
					,example = "01",style = ParameterStyle.SIMPLE)
	})
	Response buildReportByIdentifier(@QueryParam(QUERY_PARAMETER_NAME_IDENTIFIER) String identifier);
	
	@POST
	@Path(PATH_NOTIFY_ACCESS_TOKENS)
	@Consumes({MediaType.APPLICATION_JSON})
	@Produces({ MediaType.APPLICATION_JSON})
	@Operation(description = "Notifier les jetons d'accès")
	@Parameters(value = {
			@Parameter(name = "Email",allowEmptyValue = false,description = "Email",required = true,example = "k@m.com",style = ParameterStyle.SIMPLE)
	})
	Response notifyAcessTokens(@QueryParam(QUERY_PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS) String electronicMailAddress
			,@QueryParam(QUERY_PARAMETER_NAME_READ_PAGE_URL) String readPageURL);
	
	static RequestRepresentation getProxy() {
		return ProxyGetter.getInstance().get(RequestRepresentation.class);
	}
	
	String PATH = RequestBusiness.REPRESENTATION_PATH;// "demande";
	String PATH_GET_ONE_TO_BE_CREATED_BY_TYPE_IDENTIFIER = "getonetobecreatedbytypeidentifier";
	String PATH_INITIALIZE = "initialiser";
	String PATH_RECORD = "enregistrer";
	String PATH_RECORD_PHOTO = "enregistrerphoto";
	String PATH_RECORD_PHOTO_BY_IDENTIFIER = "enregistrerphotoparidentifiant";
	String PATH_RECORD_ACT_OF_APPOINTMENT = "enregistreractenomination";
	String PATH_RECORD_ACT_OF_APPOINTMENT_BY_IDENTIFIER = "enregistreractenominationparidentifiant";
	String PATH_RECORD_SIGNATURE = "enregistrersignature";
	String PATH_RECORD_SIGNATURE_BY_IDENTIFIER = "enregistrersignatureparidentifiant";
	String PATH_RECORD_SIGNED_REQUEST_SHEET = "enregistrerfichedemandesignee";
	String PATH_RECORD_SIGNED_REQUEST_SHEET_BY_IDENTIFIER = "enregistrerfichedemandesigneeparidentifiant";
	String PATH_RECORD_SIGNED_REQUEST_SHEET_BY_IDENTIFIER_FOR_ADMIN = "enregistrerfichedemandesigneeparidentifiantforadmin";
	String PATH_SUBMIT_BY_IDENTIFIER = "soumettreparidentifiant";
	String PATH_ACCEPT_BY_IDENTIFIER = "accepterparidentifiant";
	String PATH_REJECT_BY_IDENTIFIER = "rejeterparidentifiant";
	String PATH_GET_PHOTO_BY_IDENTIFIER = "obtenirphotoparidentifiant";
	String PATH_GET_ACT_OF_APPOINTMENT_BY_IDENTIFIER = "obteniractenominationparidentifiant";
	String PATH_GET_SIGNATURE_BY_IDENTIFIER = "obtenirsignatureparidentifiant";
	String PATH_GET_SIGNED_REQUEST_SHEET_BY_IDENTIFIER = "obtenirfichedemandesigneeparidentifiant";
	
	String PATH_NOTIFY_ACCESS_TOKENS = "notifyaccesstokens";
	
	String PATH_BUILD_REPORT_BY_IDENTIFIER = RequestBusiness.REPRESENTATION_PATH_BUILD_REPORT_BY_IDENTIFIER;// "construireetatparidentifiant";
	
	String TAG = "Demande";
	
	String QUERY_PARAMETER_NAME_COMMENT = "commentaire";
	String QUERY_PARAMETER_NAME_TYPE_IDENTIFIER = "type_identifiant";
	String QUERY_PARAMETER_NAME_REJECTION_REASON = "motif_rejet";
	String QUERY_PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS = "email";
	String QUERY_PARAMETER_NAME_READ_PAGE_URL = "url_page_consultation";
	String QUERY_PARAMETER_NAME_BUDGETAIRES_SCOPE_FUNCTIONS_IDENTIFIERS = "identifiants_postes_budgetaires";
}