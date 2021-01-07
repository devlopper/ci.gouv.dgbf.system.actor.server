package ci.gouv.dgbf.system.actor.server.business.api;

import org.cyk.utility.server.business.BusinessEntity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;

public interface ExecutionImputationBusiness extends BusinessEntity<ExecutionImputation> {

	String REFRESH_MATERIALIZED_VIEW = "refreshMaterializedView";
}