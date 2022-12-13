package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

public class ReqSecAsCodAdmtvUnitAsCodStatusCrtionDatProcesngDatAsStrngsReader extends ReqSecAsCodAdmtvUnitTypStatusCrtionDatPrsngDatAsStrgsReader implements Serializable {

	@Override
	protected String getAdministrativeUnitProjection() {
		return "au.code";
	}
}