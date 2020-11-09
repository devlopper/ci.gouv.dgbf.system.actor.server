package ci.gouv.dgbf.system.actor.server.representation.impl;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.number.NumberHelper;

import ci.gouv.dgbf.system.actor.server.business.impl.ExecutionImputationBusinessImpl;
import ci.gouv.dgbf.system.actor.server.business.impl.ScopeFunctionExecutionImputationBusinessImpl;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

@Path(SystemServerRepresentation.PATH)
@Api
public class SystemServerRepresentation {

	@POST
	@Path("setstatics")
	@Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Set statics",tags = {TAG})
	public Response setStatics(
			@QueryParam("DERIVE_ALL_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE") String a
			,@QueryParam("DERIVE_ALL_EXECUTION_IMPUTATIONS_PROCESS_BATCH_SIZE") String b
			,@QueryParam("DERIVE_SCOPE_FUNCTIONS_FROM_MODEL_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE") String c
			,@QueryParam("DERIVE_SCOPE_FUNCTIONS_FROM_MODEL_EXECUTION_IMPUTATIONS_PROCESS_BATCH_SIZE") String d
			) {
		ScopeFunctionExecutionImputationBusinessImpl.DERIVE_ALL_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE = NumberHelper.getInteger(a, ScopeFunctionExecutionImputationBusinessImpl.DERIVE_ALL_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE);
		ScopeFunctionExecutionImputationBusinessImpl.DERIVE_ALL_EXECUTION_IMPUTATIONS_PROCESS_BATCH_SIZE = NumberHelper.getInteger(b, ScopeFunctionExecutionImputationBusinessImpl.DERIVE_ALL_EXECUTION_IMPUTATIONS_PROCESS_BATCH_SIZE);
		ExecutionImputationBusinessImpl.DERIVE_SCOPE_FUNCTIONS_FROM_MODEL_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE = NumberHelper.getInteger(a, ExecutionImputationBusinessImpl.DERIVE_SCOPE_FUNCTIONS_FROM_MODEL_EXECUTION_IMPUTATIONS_READ_BATCH_SIZE);
		ExecutionImputationBusinessImpl.DERIVE_SCOPE_FUNCTIONS_FROM_MODEL_EXECUTION_IMPUTATIONS_PROCESS_BATCH_SIZE = NumberHelper.getInteger(b, ExecutionImputationBusinessImpl.DERIVE_SCOPE_FUNCTIONS_FROM_MODEL_EXECUTION_IMPUTATIONS_PROCESS_BATCH_SIZE);
		return Response.ok().build();
	}
	
	public static final String PATH = "system/server";
	
	public static final String TAG = "Gestion interne du système";
}